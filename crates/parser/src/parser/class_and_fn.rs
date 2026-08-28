use self::expression::BlockStmtOrExpr;

use super::*;
use crate::{context::ContextFlags, error::SyntaxError};
use expression::MaybeParen;
use util::AssignProps;

/// Parser for function expression and function declaration.
impl Parser<'_> {
    pub(super) fn parse_async_fn_expr(&mut self) -> PResult<Box<Expr>> {
        let start = self.input.cur_pos();
        expect!(self, "async");

        let (span, ident, f) = self
            .parse_fn_or_ts_overload_sig(None, Some(start), true, true)?
            .expect("Error already handled for overload sig");

        Ok(Box::new(Expr::Fn(FnExpr {
            ident,
            function: Box::new(f),
            node_id: node_id!(self, span),
        })))
    }

    /// Parse function expression
    pub(super) fn parse_fn_expr(&mut self) -> PResult<Box<Expr>> {
        let (span, ident, f) = self
            .parse_fn_or_ts_overload_sig(None, None, true, true)?
            .expect("Error already handled for overload sig");

        Ok(Box::new(Expr::Fn(FnExpr {
            ident,
            function: Box::new(f),
            node_id: node_id!(self, span),
        })))
    }

    pub(super) fn parse_async_fn_decl(&mut self) -> PResult<Option<Decl>> {
        let start = self.input.cur_pos();
        expect!(self, "async");
        self.parse_fn_or_ts_overload_sig(None, Some(start), false, false)
            .map(|res| {
                res.map(|(span, ident, f)| {
                    Decl::Fn(FnDecl {
                        ident: ident.unwrap(),
                        function: Box::new(f),
                        node_id: node_id!(self, span),
                    })
                })
            })
    }

    pub(super) fn parse_fn_decl(&mut self) -> PResult<Decl> {
        let (span, ident, f) = self
            .parse_fn_or_ts_overload_sig(None, None, false, false)?
            .expect("Error already handled for overload sig");
        Ok(Decl::Fn(FnDecl {
            ident: ident.unwrap(),
            function: Box::new(f),
            node_id: node_id!(self, span),
        }))
    }

    pub(super) fn parse_fn_decl_or_ts_overload_sig(&mut self) -> PResult<Option<Decl>> {
        self.parse_fn_or_ts_overload_sig(None, None, false, false)
            .map(|res| {
                res.map(|(span, ident, f)| {
                    Decl::Fn(FnDecl {
                        ident: ident.unwrap(),
                        function: Box::new(f),
                        node_id: node_id!(self, span),
                    })
                })
            })
    }

    pub(super) fn parse_default_async_fn(
        &mut self,
        start: BytePos,
    ) -> PResult<Option<ExportDefaultDecl>> {
        let start_of_async = self.input.cur_pos();
        expect!(self, "async");
        self.parse_fn_or_ts_overload_sig(Some(start), Some(start_of_async), false, true)
            .map(|res| {
                res.map(|(span, ident, f)| ExportDefaultDecl {
                    decl: DefaultDecl::Fn(FnExpr {
                        ident,
                        function: Box::new(f),
                        node_id: node_id!(self, span),
                    }),
                    node_id: node_id!(self, span),
                })
            })
    }

    pub(super) fn parse_default_fn(
        &mut self,
        start: BytePos,
    ) -> PResult<Option<ExportDefaultDecl>> {
        self.parse_fn_or_ts_overload_sig(Some(start), None, false, true)
            .map(|res| {
                res.map(|(span, ident, f)| ExportDefaultDecl {
                    decl: DefaultDecl::Fn(FnExpr {
                        ident,
                        function: Box::new(f),
                        node_id: node_id!(self, span),
                    }),
                    node_id: node_id!(self, span),
                })
            })
    }

    pub(super) fn parse_class_decl(
        &mut self,
        start: BytePos,
        class_start: BytePos,
    ) -> PResult<Decl> {
        let (span, ident, class) = self.parse_class(start, class_start, false)?;
        Ok(Decl::Class(ClassDecl {
            ident: ident.unwrap(),
            class: Box::new(class),
            node_id: node_id!(self, span),
        }))
    }

    pub(super) fn parse_class_expr(&mut self, start: BytePos) -> PResult<Box<Expr>> {
        let (span, ident, class) = self.parse_class(start, start, true)?;
        Ok(Box::new(Expr::Class(Box::new(ClassExpr {
            ident,
            class,
            node_id: node_id!(self, span),
        }))))
    }

    pub(super) fn parse_default_class(
        &mut self,
        start: BytePos,
        class_start: BytePos,
    ) -> PResult<ExportDefaultDecl> {
        let (span, ident, class) = self.parse_class(start, class_start, true)?;
        Ok(ExportDefaultDecl {
            decl: DefaultDecl::Class(ClassExpr {
                ident,
                class,
                node_id: node_id!(self, span),
            }),
            node_id: node_id!(self, span),
        })
    }

    fn parse_class(
        &mut self,
        start: BytePos,
        class_start: BytePos,
        name_is_optional: bool,
    ) -> PResult<(Span, Option<Ident>, Class)> {
        self.strict_mode().parse_with(|parser| {
            expect!(parser, "class");

            let ident = if name_is_optional {
                parser.parse_opt_binding_ident()?
            } else {
                Some(parser.parse_binding_ident()?)
            };

            if let Some(i) = &ident {
                if i.name == id_for_built_in!("any") {
                    parser.emit_err(get_span!(parser, (&i).node_id), SyntaxError::TS2414);
                }
            }

            // Type params.
            if parser.syntax().typescript() {
                parser.try_eat_ts_type_params(|_, _| {})?;
            }

            let mut extends_clause = if parser.is(tok!("extends")) {
                let start = parser.input.cur_pos();
                parser.input.bump();
                let super_class = parser.parse_lhs_expr(&mut AssignProps::Emit)?.unwrap();
                // Super type params.
                if parser.syntax().typescript() && parser.is(tok!('<')) {
                    parser.parse_ts_type_args()?;
                }
                let span = parser.span(start);

                if parser.syntax().typescript() && parser.eat(tok!(',')) {
                    parser.eat_ts_heritage_clause(|parser, span| {
                        parser.emit_err(span, SyntaxError::TS1174);
                    })?;
                }

                Some(ExtendsClause {
                    node_id: node_id!(parser, span),
                    super_class,
                })
            } else {
                None
            };

            // Handle TS1172
            if parser.eat(tok!("extends")) {
                parser.emit_err(parser.input.prev_span(), SyntaxError::TS1172);

                parser.parse_lhs_expr(&mut AssignProps::Emit)?;
                if parser.syntax().typescript() && parser.is(tok!('<')) {
                    parser.parse_ts_type_args()?;
                }
            };

            if parser.syntax().typescript() && parser.eat(tok!("implements")) {
                parser.eat_ts_heritage_clause(|_, _| {})?;
            }

            {
                // Handle TS1175
                if parser.syntax().typescript() && parser.eat(tok!("implements")) {
                    parser.emit_err(parser.input.prev_span(), SyntaxError::TS1175);

                    parser.eat_ts_heritage_clause(|_, _| {})?;
                }
            }

            // Handle TS1173
            if parser.syntax().typescript() && parser.is(tok!("extends")) {
                parser.emit_err(parser.input.cur_span(), SyntaxError::TS1173);
                let start = parser.input.cur_pos();
                parser.input.bump();

                let super_class = parser.parse_lhs_expr(&mut AssignProps::Emit)?.unwrap();
                // Super type params.
                if parser.syntax().typescript() && parser.is(tok!('<')) {
                    parser.parse_ts_type_args()?;
                }

                if extends_clause.is_none() {
                    extends_clause = Some(ExtendsClause {
                        node_id: node_id!(parser, parser.span(start)),
                        super_class,
                    });
                }
            }

            expect!(parser, '{');
            let mut ctx = parser.ctx();
            ctx.flags
                .set(ContextFlags::has_super_class, extends_clause.is_some());
            let body = parser.with_ctx(ctx).parse_class_body()?;
            expect!(parser, '}');
            let end = parser.input.last_pos();
            Ok((
                parser.span(start),
                ident,
                Class {
                    node_id: node_id!(parser, Span::new(class_start, end)),
                    extends: extends_clause,
                    body,
                },
            ))
        })
    }

    fn parse_class_body(&mut self) -> PResult<Vec<ClassMember>> {
        let mut elems = vec![];
        while !self.is(tok!('}')) {
            if self.eat(tok!(';')) {
                continue;
            }

            if let Some(element) = self.parse_class_member()? {
                elems.push(element);
            }
        }
        Ok(elems)
    }

    pub(super) fn parse_access_modifier(&mut self) -> PResult<bool> {
        debug_assert!(self.syntax().typescript());
        Ok(self
            .parse_ts_modifier(&[Token::Public, Token::Protected, Token::Private])?
            .is_some())
    }

    fn parse_class_member(&mut self) -> PResult<Option<ClassMember>> {
        let start = self.input.cur_pos();
        let declare = self.syntax().typescript() && self.eat(tok!("declare"));
        let has_accessibility = if self.syntax().typescript() {
            self.parse_access_modifier()?
        } else {
            false
        };
        // Allow `private declare`.
        let declare = declare || self.syntax().typescript() && self.eat(tok!("declare"));

        let declare_token = if declare {
            // Handle declare(){}
            if self.is_class_method() {
                let key = Key::PropName(PropName::Ident(
                    self.new_ident(id_for_built_in!("declare"), self.span(start)),
                ));
                // TS optional.
                if self.syntax().typescript() {
                    self.eat(tok!('?'));
                }
                return self.make_method(
                    Parser::parse_unique_formal_params,
                    MakeMethodArgs {
                        start,
                        is_abstract: false,
                        is_async: false,
                        is_generator: false,
                        static_token: None,
                        key,
                        kind: MethodKind::Method,
                    },
                );
            } else if self.is_class_property() {
                // Property named `declare`

                let key = Key::PropName(PropName::Ident(
                    self.new_ident(id_for_built_in!("declare"), self.span(start)),
                ));
                let is_optional = self.syntax().typescript() && self.eat(tok!('?'));
                return self.make_property(start, key, false, is_optional, false, false);
            } else {
                Some(self.span(start))
            }
        } else {
            None
        };

        let static_token = {
            let start = self.input.cur_pos();
            if self.eat(tok!("static")) {
                Some(self.span(start))
            } else {
                None
            }
        };

        if let Some(static_token) = static_token {
            // Handle static(){}
            if self.is_class_method() {
                let key = Key::PropName(PropName::Ident(
                    self.new_ident(id_for_built_in!("static"), static_token),
                ));
                // TS optional.
                if self.syntax().typescript() {
                    self.eat(tok!('?'));
                }
                return self.make_method(
                    Parser::parse_unique_formal_params,
                    MakeMethodArgs {
                        start,
                        is_abstract: false,
                        is_async: false,
                        is_generator: false,
                        static_token: None,
                        key,
                        kind: MethodKind::Method,
                    },
                );
            } else if self.is_class_property() {
                // Property named `static`

                let key = Key::PropName(PropName::Ident(
                    self.new_ident(id_for_built_in!("static"), static_token),
                ));
                let is_optional = self.syntax().typescript() && self.eat(tok!('?'));
                return self.make_property(start, key, false, is_optional, declare, false);
            } else {
                // TODO: error if static contains escape
            }
        }

        self.parse_class_member_with_is_static(
            start,
            declare_token,
            has_accessibility,
            static_token,
        )
    }

    #[allow(clippy::cognitive_complexity)]
    fn parse_class_member_with_is_static(
        &mut self,
        start: BytePos,
        declare_token: Option<Span>,
        has_accessibility: bool,
        static_token: Option<Span>,
    ) -> PResult<Option<ClassMember>> {
        let mut is_static = static_token.is_some();

        let mut is_abstract = false;
        let mut is_override = false;
        let mut readonly = None;
        let mut modifier_span = None;
        let declare = declare_token.is_some();
        if self.syntax().typescript() {
            while let Some(modifier) = self.parse_ts_modifier(&[
                Token::Abstract,
                Token::Readonly,
                Token::Override,
                Token::Static,
            ])? {
                modifier_span = Some(self.input.prev_span());
                match modifier {
                    Token::Abstract => {
                        if is_abstract {
                            self.emit_err(self.input.prev_span(), SyntaxError::TS1030("abstract"));
                        } else if is_override {
                            self.emit_err(
                                self.input.prev_span(),
                                SyntaxError::TS1029("abstract", "override"),
                            );
                        } else {
                            is_abstract = true;
                        }
                    }
                    Token::Override => {
                        if is_override {
                            self.emit_err(self.input.prev_span(), SyntaxError::TS1030("override"));
                        } else if readonly.is_some() {
                            self.emit_err(
                                self.input.prev_span(),
                                SyntaxError::TS1029("override", "readonly"),
                            );
                        } else if declare {
                            self.emit_err(
                                self.input.prev_span(),
                                SyntaxError::TS1243("override", "declare"),
                            );
                        } else if !self.ctx().has_super_class() {
                            self.emit_err(self.input.prev_span(), SyntaxError::TS4112);
                        } else {
                            is_override = true;
                        }
                    }
                    Token::Readonly => {
                        let readonly_span = self.input.prev_span();
                        if readonly.is_some() {
                            self.emit_err(readonly_span, SyntaxError::TS1030("readonly"));
                        } else {
                            readonly = Some(readonly_span);
                        }
                    }
                    Token::Static => {
                        if is_override {
                            self.emit_err(
                                self.input.prev_span(),
                                SyntaxError::TS1029("static", "override"),
                            );
                        }

                        is_static = true;
                    }
                    _ => {}
                }
            }
        }

        if self.syntax().typescript() && !is_abstract && !is_override && !has_accessibility {
            let idx = self.try_parse_ts_index_signature()?;
            if idx.is_some() {
                return Ok(None);
            }
        }

        if self.eat(tok!('*')) {
            // generator method
            let key = self.parse_class_prop_name()?;
            if readonly.is_some() {
                self.emit_err(self.span(start), SyntaxError::ReadOnlyMethod);
            }
            if is_constructor(&key) {
                self.emit_err(self.span(start), SyntaxError::GeneratorConstructor);
            }

            return self.make_method(
                Parser::parse_unique_formal_params,
                MakeMethodArgs {
                    start,
                    is_async: false,
                    is_generator: true,
                    is_abstract,
                    static_token,
                    key,
                    kind: MethodKind::Method,
                },
            );
        }

        let key = if let Some(readonly) = readonly
            && (self.is(tok!('!')) || self.is(tok!(':')))
        {
            Key::PropName(PropName::Ident(
                self.new_ident(id_for_built_in!("readonly"), readonly),
            ))
        } else {
            self.parse_class_prop_name()?
        };
        let is_optional = self.syntax().typescript() && self.eat(tok!('?'));

        if self.is_class_method() {
            // handle a(){} / get(){} / set(){} / async(){}

            if let Some(token) = declare_token {
                self.emit_err(token, SyntaxError::TS1031);
            }

            if readonly.is_some() {
                syntax_error!(self, self.span(start), SyntaxError::ReadOnlyMethod);
            }
            let is_constructor = is_constructor(&key);

            if is_constructor {
                if self.syntax().typescript() && is_override {
                    self.emit_err(self.span(start), SyntaxError::TS1089("override"));
                }

                if self.syntax().typescript() && self.is(tok!('<')) {
                    let start = self.input.cur_pos();
                    if self.peeked_is(tok!('>')) {
                        self.assert_and_bump(tok!('<'));
                        let start2 = self.input.cur_pos();
                        self.assert_and_bump(tok!('>'));

                        self.emit_err(self.span(start), SyntaxError::TS1098);
                        self.emit_err(self.span(start2), SyntaxError::TS1092);
                    } else {
                        self.try_eat_ts_type_params(|p, span| {
                            p.emit_err(span, SyntaxError::TS1092);
                        })?;
                    }
                }

                expect!(self, '(');
                // TODO: param props
                let (params, param_props) = self.parse_constructor_params()?;
                expect!(self, ')');

                if self.syntax().typescript() && self.is(tok!(':')) {
                    let type_ann_span = self.parse_ts_type_ann(true)?;

                    self.emit_err(type_ann_span, SyntaxError::TS1093);
                }

                let body: Option<_> = self.parse_fn_body(false, false)?;

                if body.is_none() {
                    if let Some(last) = param_props.last().map(|(_, p)| p) {
                        for (_, param) in &param_props {
                            if param != last {
                                self.emit_err(*param, SyntaxError::TS2369);
                            }
                        }
                        syntax_error!(self, *last, SyntaxError::TS2369);
                    }
                }

                if self.syntax().typescript() && body.is_none() {
                    // Declare constructors cannot have assignment pattern in parameters
                    for p in &params {
                        // TODO: Search deeply for assignment pattern using a Visitor

                        let span = match &p.pat {
                            Pat::Assign(p) => Some(get_span!(self, p.node_id())),
                            _ => None,
                        };

                        if let Some(span) = span {
                            self.emit_err(span, SyntaxError::TS2371);
                        }
                    }
                }

                if self.syntax().typescript() {
                    if let Some(static_token) = static_token {
                        self.emit_err(static_token, SyntaxError::TS1089("static"));
                    }
                }

                if let Some(span) = modifier_span {
                    if is_abstract {
                        self.emit_err(span, SyntaxError::TS1242);
                    }
                }

                let Some(body) = body else {
                    if self.syntax().typescript() {
                        if param_props.is_empty() {
                            return Ok(None);
                        }

                        unreachable!("should have thrown error above");
                    } else {
                        unreachable!("parse_fn_body should have returned Err");
                    }
                };

                return Ok(Some(ClassMember::Constructor(Constructor {
                    node_id: node_id!(self, self.span(start)),
                    params,
                    body,
                })));
            } else {
                return self.make_method(
                    Parser::parse_formal_params,
                    MakeMethodArgs {
                        start,
                        is_abstract,
                        static_token,
                        kind: MethodKind::Method,
                        key,
                        is_async: false,
                        is_generator: false,
                    },
                );
            }
        }

        if self.is_class_property() {
            return self.make_property(start, key, is_static, is_optional, declare, is_abstract);
        }

        if match &key {
            Key::PropName(PropName::Ident(i)) => i.name == id_for_built_in!("async"),
            _ => false,
        } && !self.input.had_line_break_before_cur()
        {
            // handle async foo(){}

            if self.syntax().typescript() && self.parse_ts_modifier(&[Token::Override])?.is_some() {
                self.emit_err(
                    self.input.prev_span(),
                    SyntaxError::TS1029("override", "async"),
                );
            }

            let is_generator = self.eat(tok!('*'));
            let key = self.parse_class_prop_name()?;
            if is_constructor(&key) {
                syntax_error!(
                    self,
                    get_span!(self, key.node_id()),
                    SyntaxError::AsyncConstructor
                )
            }
            if readonly.is_some() {
                syntax_error!(self, self.span(start), SyntaxError::ReadOnlyMethod);
            }

            // handle async foo(){}

            // TS optional.
            if !is_optional && self.syntax().typescript() {
                self.eat(tok!('?'));
            }
            return self.make_method(
                Parser::parse_unique_formal_params,
                MakeMethodArgs {
                    start,
                    static_token,
                    key,
                    is_abstract,
                    kind: MethodKind::Method,
                    is_async: true,
                    is_generator,
                },
            );
        }

        let is_next_line_generator = self.input.had_line_break_before_cur() && self.is(tok!('*'));
        let key_span = get_span!(self, key.node_id());

        match &key {
            // `get\n*` is an uninitialized property named 'get' followed by a generator.
            Key::PropName(PropName::Ident(i))
                if (i.name == id_for_built_in!("get") || i.name == id_for_built_in!("set"))
                    && !is_next_line_generator =>
            {
                // handle get foo(){} / set foo(v){}
                let key = self.parse_class_prop_name()?;

                if readonly.is_some() {
                    self.emit_err(key_span, SyntaxError::GetterSetterCannotBeReadonly);
                }

                return match i.name {
                    id_for_built_in!("get") => self.make_method(
                        Parser::parse_formal_params,
                        MakeMethodArgs {
                            start,
                            is_abstract,
                            is_async: false,
                            is_generator: false,
                            static_token,
                            key,
                            kind: MethodKind::Getter,
                        },
                    ),
                    id_for_built_in!("set") => self.make_method(
                        Parser::parse_formal_params,
                        MakeMethodArgs {
                            start,
                            is_abstract,
                            is_async: false,
                            is_generator: false,
                            static_token,
                            key,
                            kind: MethodKind::Setter,
                        },
                    ),
                    _ => unreachable!(),
                };
            }
            _ => {}
        }

        unexpected!(self, "* for generator, private key, identifier or async")
    }

    fn make_property(
        &mut self,
        start: BytePos,
        key: Key,
        is_static: bool,
        is_optional: bool,
        declare: bool,
        is_abstract: bool,
    ) -> PResult<Option<ClassMember>> {
        if is_constructor(&key) {
            syntax_error!(
                self,
                get_span!(self, key.node_id()),
                SyntaxError::PropertyNamedConstructor
            );
        }
        if declare && matches!(key, Key::PrivateName(_)) {
            syntax_error!(
                self,
                get_span!(self, key.node_id()),
                SyntaxError::DeclarePrivateIdentifier
            );
        }
        // TS definite.
        if self.syntax().typescript() && !is_optional {
            self.eat(tok!('!'));
        }

        // Type annotation.
        self.try_parse_ts_type_ann()?;

        let ctx = Context {
            flags: (self.ctx().flags | ContextFlags::in_class_prop | ContextFlags::include_in_expr),
            ..self.ctx()
        };
        self.with_ctx(ctx).parse_with(|parser| {
            let value = if parser.is(tok!('=')) {
                parser.assert_and_bump(tok!('='));
                Some(
                    parser
                        .parse_assignment_expr(&mut AssignProps::Emit)?
                        .unwrap(),
                )
            } else {
                None
            };

            if !parser.eat_semi_with_asi() {
                parser.emit_err(parser.input.cur_span(), SyntaxError::TS1005);
            }

            if is_abstract || declare {
                return Ok(None);
            }

            Ok(Some(match key {
                Key::PrivateName(key) => ClassMember::PrivateProp(PrivateProp {
                    node_id: node_id!(parser, parser.span(start)),
                    key,
                    value,
                    is_static,
                }),
                Key::PropName(key) => ClassMember::ClassProp(ClassProp {
                    node_id: node_id!(parser, parser.span(start)),
                    key,
                    value,
                    is_static,
                }),
            }))
        })
    }

    fn is_class_method(&mut self) -> bool {
        self.is(tok!('(')) || (self.syntax().typescript() && self.is(tok!('<')))
    }

    fn is_class_property(&mut self) -> bool {
        (self.syntax().typescript() && (self.is(tok!('!')) || self.is(tok!(':'))))
            || (self.is(tok!('=')) || self.is_semi_with_asi() || self.is(tok!('}')))
    }

    fn parse_fn_or_ts_overload_sig(
        &mut self,
        start_of_output_type: Option<BytePos>,
        start_of_async: Option<BytePos>,
        is_fn_expr: bool,
        name_is_optional: bool,
    ) -> PResult<Option<(Span, Option<Ident>, Function)>> {
        let start = start_of_async.unwrap_or_else(|| self.input.cur_pos());
        self.assert_and_bump(tok!("function"));
        let is_async = start_of_async.is_some();

        let is_generator = {
            if self.eat(tok!('*')) {
                // if is_async {
                //     syntax_error!(self, self.span(start), SyntaxError::AsyncGenerator {});
                // }
                true
            } else {
                false
            }
        };

        let mut ctx = self.ctx();
        ctx.flags.set(ContextFlags::in_async, is_async);
        ctx.flags.set(ContextFlags::in_generator, is_generator);

        // From babel..
        //
        // When parsing function expression, the binding identifier is parsed
        // according to the rules inside the function.
        // e.g. (function* yield() {}) is invalid because "yield" is disallowed
        // in generators.
        // This isn't the case with function declarations: function* yield() {}
        // is valid because yield is parsed as if it was outside the generator.
        // Therefore, this.state.inGenerator is set before or after parsing the
        // function id according to the "isStatement" parameter.
        let ident = if is_fn_expr {
            let mut ctx = ctx;
            ctx.flags.set(ContextFlags::in_generator, is_generator);
            if name_is_optional {
                self.with_ctx(ctx).parse_opt_binding_ident()?
            } else {
                Some(self.with_ctx(ctx).parse_binding_ident()?)
            }
        } else {
            // function declaration does not change context for `BindingIdentifier`.
            if name_is_optional {
                self.parse_opt_binding_ident()?
            } else {
                Some(self.parse_binding_ident()?)
            }
        };

        self.with_ctx(ctx).parse_with(|parser| {
            let f = parser.parse_fn_args_body_or_ts_overload_sig(
                start,
                Parser::parse_formal_params,
                is_async,
                is_generator,
            )?;
            // expect!(self, '(');
            // let params_ctx = Context {
            //     in_parameters: true,
            //     ..p.ctx
            // };
            // let params = p.with_ctx(params_ctx).parse_formal_params()?;
            // expect!(self, ')');

            // let body = p.parse_fn_body(is_async, is_generator)?;

            Ok(f.map(|f| (parser.span(start_of_output_type.unwrap_or(start)), ident, f)))
        })
    }

    /// `parse_args` closure should not eat '(' or ')'.
    pub(super) fn parse_fn_args_body<F>(
        &mut self,
        start: BytePos,
        parse_args: F,
        is_async: bool,
        is_generator: bool,
    ) -> PResult<Function>
    where
        F: FnOnce(&mut Self) -> PResult<Vec<Param>>,
    {
        Ok(self
            .parse_fn_args_body_or_ts_overload_sig(start, parse_args, is_async, is_generator)?
            .expect("Error already handled for overload sig"))
    }

    /// `parse_args` closure should not eat '(' or ')'.
    /// Returns `None` an overload signature was parsed.
    fn parse_fn_args_body_or_ts_overload_sig<F>(
        &mut self,
        start: BytePos,
        parse_args: F,
        is_async: bool,
        is_generator: bool,
    ) -> PResult<Option<Function>>
    where
        F: FnOnce(&mut Self) -> PResult<Vec<Param>>,
    {
        // let prev_in_generator = self.ctx().in_generator;
        let mut ctx = self.ctx();
        ctx.flags.set(ContextFlags::in_async, is_async);
        ctx.flags.set(ContextFlags::in_generator, is_generator);

        self.with_ctx(ctx).parse_with(|parser| {
            // Type params.
            if parser.syntax().typescript() {
                parser.in_type().parse_with(|parser| {
                    if parser.is(tok!('<')) {
                        parser.eat_ts_type_params(|_, _| {})?;
                    }
                    Ok(Some(()))
                })?;
            }

            expect!(parser, '(');

            let arg_ctx = Context {
                flags: parser.ctx().flags | ContextFlags::in_parameters,
                // in_generator: prev_in_generator,
                ..parser.ctx()
            };
            let params = parser
                .with_ctx(arg_ctx)
                .parse_with(|parser| parse_args(parser))?;

            expect!(parser, ')');

            // Return type
            if parser.syntax().typescript() && parser.is(tok!(':')) {
                parser
                    .parse_ts_type_or_type_predicate_ann(tok!(':'))
                    .map(Some)?;
            }

            let body: Option<_> = parser.parse_fn_body(is_async, is_generator)?;

            let body = match body {
                Some(b) => b,
                None => {
                    if parser.syntax().typescript() {
                        // Declare functions cannot have assignment pattern in parameters
                        for param in &params {
                            // TODO: Search deeply for assignment pattern using a Visitor

                            let span = match &param.pat {
                                Pat::Assign(p) => Some(get_span!(parser, p.node_id)),
                                _ => None,
                            };

                            if let Some(span) = span {
                                parser.emit_err(span, SyntaxError::TS2371);
                            }
                        }
                        return Ok(None);
                    } else {
                        unreachable!()
                    }
                }
            };

            let mut flags = FnFlags::empty();
            flags.set(FnFlags::ASYNC, is_async);
            flags.set(FnFlags::GENERATOR, is_generator);

            Ok(Some(Function {
                node_id: node_id!(parser, parser.span(start)),
                params,
                body,
                flags,
            }))
        })
    }

    fn parse_class_prop_name(&mut self) -> PResult<Key> {
        if self.is(tok!('#')) {
            self.parse_private_name().map(Key::PrivateName)
        } else {
            self.parse_prop_name().map(Key::PropName)
        }
    }

    pub(super) fn parse_fn_body<T>(&mut self, is_async: bool, is_generator: bool) -> PResult<T>
    where
        Self: FnBodyParser<T>,
    {
        if self.ctx().in_declare() && self.syntax().typescript() && self.is(tok!('{')) {
            //            self.emit_err(
            //                self.ctx().span_of_fn_name.expect("we are not in function"),
            //                SyntaxError::TS1183,
            //            );
            self.emit_err(self.input.cur_span(), SyntaxError::TS1183);
        }

        let mut ctx = Context {
            flags: (self.ctx().flags | ContextFlags::in_function)
                & !ContextFlags::is_break_allowed
                & !ContextFlags::is_continue_allowed,
            ..self.ctx()
        };
        ctx.flags.set(ContextFlags::in_async, is_async);
        ctx.flags.set(ContextFlags::in_generator, is_generator);

        let prev_labels = std::mem::take(&mut self.labels);
        let res = self.with_ctx(ctx).parse_fn_body_inner();
        self.labels = prev_labels;
        res
    }
}

impl Parser<'_> {
    fn make_method<F>(
        &mut self,
        parse_args: F,
        MakeMethodArgs {
            start,
            is_abstract,
            static_token,
            key,
            kind,
            is_async,
            is_generator,
        }: MakeMethodArgs,
    ) -> PResult<Option<ClassMember>>
    where
        F: FnOnce(&mut Self) -> PResult<Vec<Param>>,
    {
        let is_static = static_token.is_some();

        let function =
            self.parse_fn_args_body_or_ts_overload_sig(start, parse_args, is_async, is_generator)?;

        match kind {
            MethodKind::Getter | MethodKind::Setter
                if self.syntax().typescript() && self.input.target() == JscTarget::Es3 =>
            {
                self.emit_err(get_span!(self, key.node_id()), SyntaxError::TS1056);
            }
            _ => {}
        }

        if is_abstract {
            return Ok(None);
        }

        let Some(function) = function else {
            return Ok(None);
        };

        if kind == MethodKind::Getter {
            for param in &function.params {
                self.emit_err(get_span!(self, param.node_id), SyntaxError::GetterParam);
            }
        }

        if kind == MethodKind::Setter {
            if function.params.len() != 1 {
                self.emit_err(get_span!(self, key.node_id()), SyntaxError::SetterParam);
            }

            if !function.params.is_empty() {
                if let Pat::Rest(first) = &function.params[0].pat {
                    self.emit_err(get_span!(self, first.node_id), SyntaxError::RestPatInSetter);
                }
            }
        }

        Ok(Some(match key {
            Key::PrivateName(key) => ClassMember::PrivateMethod(PrivateMethod {
                node_id: node_id!(self, self.span(start)),

                is_static,
                key,
                function,
                kind,
            }),
            Key::PropName(key) => ClassMember::Method(ClassMethod {
                node_id: node_id!(self, self.span(start)),

                is_static,
                key,
                function,
                kind,
            }),
        }))
    }
}

pub(super) trait FnBodyParser<Body> {
    fn parse_fn_body_inner(&mut self) -> PResult<Body>;
}

impl FnBodyParser<BlockStmtOrExpr> for Parser<'_> {
    fn parse_fn_body_inner(&mut self) -> PResult<BlockStmtOrExpr> {
        if self.is(tok!('{')) {
            self.parse_block(false).map(BlockStmtOrExpr::BlockStmt)
        } else {
            self.parse_assignment_expr(&mut AssignProps::Emit)
                .map(MaybeParen::unwrap)
                .map(BlockStmtOrExpr::Expr)
        }
    }
}

impl FnBodyParser<Option<BlockStmt>> for Parser<'_> {
    fn parse_fn_body_inner(&mut self) -> PResult<Option<BlockStmt>> {
        // allow omitting body and allow placing `{` on next line
        if self.input.syntax().typescript() && !self.is(tok!('{')) && self.eat_semi_with_asi() {
            return Ok(None);
        }
        self.include_in_expr(true).parse_block(true).map(Some)
    }
}

fn is_constructor(key: &Key) -> bool {
    match key {
        Key::PropName(PropName::Ident(Ident {
            name: id_for_built_in!("constructor"),
            ..
        })) => true,
        Key::PropName(PropName::Str(Str { value, .. })) => value.as_str() == "constructor",
        _ => false,
    }
}

struct MakeMethodArgs {
    start: BytePos,
    is_abstract: bool,
    static_token: Option<Span>,
    key: Key,
    kind: MethodKind,
    is_async: bool,
    is_generator: bool,
}

enum Key {
    PrivateName(PrivateName),
    PropName(PropName),
}

impl GetNodeId for Key {
    fn node_id(&self) -> NodeId {
        match self {
            Key::PrivateName(n) => n.node_id,
            Key::PropName(n) => n.node_id(),
        }
    }
}
