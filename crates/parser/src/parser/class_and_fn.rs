use super::{identifier::MaybeOptionalIdentParser, *};
use crate::{error::SyntaxError, lexer::TokenContext, Tokens};
use atoms::js_word;
use either::Either;
use global_common::{Spanned, SyntaxContext};

/// Parser for function expression and function declaration.
impl<I: Tokens> Parser<I> {
    pub(super) fn parse_async_fn_expr(&mut self) -> PResult<Box<Expr>> {
        let start = self.input.cur_pos();
        expect!(self, "async");
        self.parse_fn(None, Some(start), vec![])
    }

    /// Parse function expression
    pub(super) fn parse_fn_expr(&mut self) -> PResult<Box<Expr>> {
        self.parse_fn(None, None, vec![])
    }

    pub(super) fn parse_async_fn_decl(&mut self, decorators: Vec<Decorator>) -> PResult<Decl> {
        let start = self.input.cur_pos();
        expect!(self, "async");
        self.parse_fn(None, Some(start), decorators)
    }

    pub(super) fn parse_fn_decl(&mut self, decorators: Vec<Decorator>) -> PResult<Decl> {
        self.parse_fn(None, None, decorators)
    }

    pub(super) fn parse_default_async_fn(
        &mut self,
        start: BytePos,
        decorators: Vec<Decorator>,
    ) -> PResult<ExportDefaultDecl> {
        let start_of_async = self.input.cur_pos();
        expect!(self, "async");
        self.parse_fn(Some(start), Some(start_of_async), decorators)
    }

    pub(super) fn parse_default_fn(
        &mut self,
        start: BytePos,
        decorators: Vec<Decorator>,
    ) -> PResult<ExportDefaultDecl> {
        self.parse_fn(Some(start), None, decorators)
    }

    pub(super) fn parse_class_decl(
        &mut self,
        start: BytePos,
        class_start: BytePos,
        decorators: Vec<Decorator>,
    ) -> PResult<Decl> {
        self.parse_class(start, class_start, decorators)
    }

    pub(super) fn parse_class_expr(
        &mut self,
        start: BytePos,
        decorators: Vec<Decorator>,
    ) -> PResult<Box<Expr>> {
        self.parse_class(start, start, decorators)
    }

    pub(super) fn parse_default_class(
        &mut self,
        start: BytePos,
        class_start: BytePos,
        decorators: Vec<Decorator>,
    ) -> PResult<ExportDefaultDecl> {
        self.parse_class(start, class_start, decorators)
    }

    fn parse_class<T>(
        &mut self,
        start: BytePos,
        class_start: BytePos,
        decorators: Vec<Decorator>,
    ) -> PResult<T>
    where
        T: OutputType,
        Self: MaybeOptionalIdentParser<T::Ident>,
    {
        self.strict_mode().parse_with(|parser| {
            expect!(parser, "class");

            let ident = parser.parse_maybe_opt_binding_ident()?;
            if let Some(span) = ident.invalid_class_name() {
                parser.emit_err(span, SyntaxError::TS2414);
            }

            let type_params = if parser.syntax().typescript() {
                parser.try_parse_ts_type_params()?
            } else {
                None
            };

            let mut extends_clause = if is!(parser, "extends") {
                let start = parser.input.cur_pos();
                parser.input.bump();
                let super_class = parser.parse_lhs_expr()?;
                let super_type_params = if parser.syntax().typescript() && is!(parser, '<') {
                    Some(parser.parse_ts_type_args()?)
                } else {
                    None
                };
                let span = span!(parser, start);

                if parser.syntax().typescript() && eat!(parser, ',') {
                    let exprs = parser.parse_ts_heritage_clause()?;

                    for e in exprs {
                        parser.emit_err(e, SyntaxError::TS1174);
                    }
                }

                Some(ExtendsClause {
                    node_id: node_id!(parser),
                    span,
                    super_class,
                })
            } else {
                None
            };

            // Handle TS1172
            if parser.input.eat(&tok!("extends")) {
                parser.emit_err(parser.input.prev_span(), SyntaxError::TS1172);

                parser.parse_lhs_expr()?;
                if parser.syntax().typescript() && is!(parser, '<') {
                    parser.parse_ts_type_args()?;
                }
            };

            if parser.syntax().typescript() && eat!(parser, "implements") {
                parser.parse_ts_heritage_clause()?;
            }

            {
                // Handle TS1175
                if parser.syntax().typescript() && eat!(parser, "implements") {
                    parser.emit_err(parser.input.prev_span(), SyntaxError::TS1175);

                    parser.parse_ts_heritage_clause()?;
                }
            }

            // Handle TS1173
            if parser.syntax().typescript() && is!(parser, "extends") {
                parser.emit_err(parser.input.cur_span(), SyntaxError::TS1173);
                let start = parser.input.cur_pos();
                parser.input.bump();

                let super_class = parser.parse_lhs_expr()?;
                let super_type_params = if parser.syntax().typescript() && is!(parser, '<') {
                    parser.parse_ts_type_args().map(Some)?
                } else {
                    None
                };

                if extends_clause.is_none() {
                    extends_clause = Some(ExtendsClause {
                        node_id: node_id!(parser),
                        span: span!(parser, start),
                        super_class,
                    });
                }
            }

            expect!(parser, '{');
            let body = parser
                .with_ctx(Context {
                    has_super_class: extends_clause.is_some(),
                    ..parser.ctx()
                })
                .parse_class_body()?;
            expect!(parser, '}');
            let end = parser.input.last_pos();
            Ok(T::finish_class(
                span!(parser, start),
                ident,
                Class {
                    node_id: node_id!(parser),
                    span: Span::new(class_start, end, Default::default()),
                    decorators,
                    extends: extends_clause,
                    body,
                },
                node_id!(parser),
            ))
        })
    }

    pub(super) fn parse_decorators(&mut self, allow_export: bool) -> PResult<Vec<Decorator>> {
        if !self.syntax().decorators() {
            return Ok(vec![]);
        }

        let mut decorators = vec![];
        let start = self.input.cur_pos();

        while is!(self, '@') {
            decorators.push(self.parse_decorator()?);
        }
        if decorators.is_empty() {
            return Ok(decorators);
        }

        if is!(self, "export") {
            if !allow_export {
                syntax_error!(self, self.input.cur_span(), SyntaxError::ExportNotAllowed);
            }

            if !self.syntax().decorators_before_export() {
                syntax_error!(self, span!(self, start), SyntaxError::DecoratorOnExport);
            }
        } else if !is!(self, "class") {
            // syntax_error!(self, span!(self, start),
            // SyntaxError::InvalidLeadingDecorator)
        }

        Ok(decorators)
    }

    fn parse_decorator(&mut self) -> PResult<Decorator> {
        let start = self.input.cur_pos();

        self.assert_and_bump(&tok!('@'));

        let expr = if eat!(self, '(') {
            let expr = self.parse_expr()?;
            expect!(self, ')');
            expr
        } else {
            let mut expr = self
                .parse_ident(false, false)
                .map(Expr::from)
                .map(Box::new)?;

            while eat!(self, '.') {
                let ident = self.parse_ident(true, true)?;

                let span = Span::new(start, expr.span().hi(), Default::default());

                expr = Box::new(Expr::Member(MemberExpr {
                    node_id: node_id!(self),
                    span,
                    obj: ExprOrSuper::Expr(expr),
                    computed: false,
                    prop: Box::new(Expr::Ident(ident)),
                }));
            }

            expr
        };

        let expr = self.parse_maybe_decorator_args(expr)?;

        Ok(Decorator {
            node_id: node_id!(self),
            span: span!(self, start),
            expr,
        })
    }

    fn parse_maybe_decorator_args(&mut self, expr: Box<Expr>) -> PResult<Box<Expr>> {
        let type_args = if self.syntax().typescript() && is!(self, '<') {
            Some(self.parse_ts_type_args()?)
        } else {
            None
        };

        if type_args.is_none() && !is!(self, '(') {
            return Ok(expr);
        }

        let args = self.parse_args(false)?;
        Ok(Box::new(Expr::Call(CallExpr {
            node_id: node_id!(self),
            span: span!(self, expr.span().lo()),
            callee: ExprOrSuper::Expr(expr),
            args,
        })))
    }

    fn parse_class_body(&mut self) -> PResult<Vec<ClassMember>> {
        let mut elems = vec![];
        while !eof!(self) && !is!(self, '}') {
            if self.input.eat(&tok!(';')) {
                let span = self.input.prev_span();
                elems.push(ClassMember::Empty(EmptyStmt {
                    node_id: node_id!(self),
                    span: Span::new(span.lo, span.hi, SyntaxContext::empty()),
                }));
                continue;
            }

            if let Some(element) = self.parse_class_member()? {
                elems.push(element);
            }
        }
        Ok(elems)
    }

    pub(super) fn parse_access_modifier(&mut self) -> PResult<bool> {
        Ok(self
            .parse_ts_modifier(&["public", "protected", "private"])?
            .is_some())
    }

    fn parse_class_member(&mut self) -> PResult<Option<ClassMember>> {
        trace_cur!(self, parse_class_member);

        let start = self.input.cur_pos();
        let decorators = self.parse_decorators(false)?;
        let declare = self.syntax().typescript() && eat!(self, "declare");
        let has_accessibility = if self.syntax().typescript() {
            self.parse_access_modifier()?
        } else {
            false
        };
        // Allow `private declare`.
        let declare = declare || self.syntax().typescript() && eat!(self, "declare");

        let declare_token = if declare {
            // Handle declare(){}
            if self.is_class_method()? {
                let key = Either::Right(PropName::Ident(
                    self.new_ident(js_word!("declare"), span!(self, start)),
                ));
                let is_optional = self.syntax().typescript() && eat!(self, '?');
                return self.make_method(
                    |parser| parser.parse_unique_formal_params(),
                    MakeMethodArgs {
                        start,
                        decorators,
                        is_abstract: false,
                        is_optional,
                        is_override: false,
                        is_async: false,
                        is_generator: false,
                        static_token: None,
                        key,
                        kind: MethodKind::Method,
                    },
                );
            } else if self.is_class_property()? {
                // Property named `declare`

                let key = Either::Right(PropName::Ident(
                    self.new_ident(js_word!("declare"), span!(self, start)),
                ));
                let is_optional = self.syntax().typescript() && eat!(self, '?');
                return self.make_property(
                    start,
                    decorators,
                    key,
                    false,
                    is_optional,
                    false,
                    false,
                    false,
                    false,
                );
            } else {
                Some(span!(self, start))
            }
        } else {
            None
        };

        let static_token = {
            let start = self.input.cur_pos();
            if self.input.eat(&tok!("static")) {
                Some(span!(self, start))
            } else {
                None
            }
        };

        if let Some(static_token) = static_token {
            // Handle static(){}
            if self.is_class_method()? {
                let key = Either::Right(PropName::Ident(
                    self.new_ident(js_word!("static"), static_token),
                ));
                let is_optional = self.syntax().typescript() && eat!(self, '?');
                return self.make_method(
                    |parser| parser.parse_unique_formal_params(),
                    MakeMethodArgs {
                        start,
                        decorators,
                        is_abstract: false,
                        is_optional,
                        is_override: false,
                        is_async: false,
                        is_generator: false,
                        static_token: None,
                        key,
                        kind: MethodKind::Method,
                    },
                );
            } else if self.is_class_property()? {
                // Property named `static`

                let key = Either::Right(PropName::Ident(
                    self.new_ident(js_word!("static"), static_token),
                ));
                let is_optional = self.syntax().typescript() && eat!(self, '?');
                return self.make_property(
                    start,
                    decorators,
                    key,
                    false,
                    is_optional,
                    false,
                    declare,
                    false,
                    false,
                );
            } else {
                // TODO: error if static contains escape
            }
        }

        self.parse_class_member_with_is_static(
            start,
            declare_token,
            has_accessibility,
            static_token,
            decorators,
        )
    }

    #[allow(clippy::cognitive_complexity)]
    fn parse_class_member_with_is_static(
        &mut self,
        start: BytePos,
        declare_token: Option<Span>,
        has_accessibility: bool,
        static_token: Option<Span>,
        decorators: Vec<Decorator>,
    ) -> PResult<Option<ClassMember>> {
        let mut is_static = static_token.is_some();

        let mut is_abstract = false;
        let mut is_override = false;
        let mut readonly = None;
        let mut modifier_span = None;
        let declare = declare_token.is_some();
        while let Some(modifier) =
            self.parse_ts_modifier(&["abstract", "readonly", "override", "static"])?
        {
            modifier_span = Some(self.input.prev_span());
            match modifier {
                "abstract" => {
                    if is_abstract {
                        self.emit_err(
                            self.input.prev_span(),
                            SyntaxError::TS1030(js_word!("abstract")),
                        );
                    } else if is_override {
                        self.emit_err(
                            self.input.prev_span(),
                            SyntaxError::TS1029(js_word!("abstract"), js_word!("override")),
                        );
                    } else {
                        is_abstract = true;
                    }
                }
                "override" => {
                    if is_override {
                        self.emit_err(
                            self.input.prev_span(),
                            SyntaxError::TS1030(js_word!("override")),
                        );
                    } else if readonly.is_some() {
                        self.emit_err(
                            self.input.prev_span(),
                            SyntaxError::TS1029(js_word!("override"), js_word!("readonly")),
                        );
                    } else if declare {
                        self.emit_err(
                            self.input.prev_span(),
                            SyntaxError::TS1243(js_word!("override"), js_word!("declare")),
                        );
                    } else if !self.ctx().has_super_class {
                        self.emit_err(self.input.prev_span(), SyntaxError::TS4112);
                    } else {
                        is_override = true;
                    }
                }
                "readonly" => {
                    let readonly_span = self.input.prev_span();
                    if readonly.is_some() {
                        self.emit_err(readonly_span, SyntaxError::TS1030(js_word!("readonly")));
                    } else {
                        readonly = Some(readonly_span);
                    }
                }
                "static" => {
                    if is_override {
                        self.emit_err(
                            self.input.prev_span(),
                            SyntaxError::TS1029(js_word!("static"), js_word!("override")),
                        );
                    }

                    is_static = true;
                }
                _ => {}
            }
        }

        if self.syntax().typescript() && !is_abstract && !is_override && !has_accessibility {
            let idx = self.try_parse_ts_index_signature(start, readonly.is_some(), is_static)?;
            if let Some(idx) = idx {
                return Ok(None);
            }
        }

        if eat!(self, '*') {
            // generator method
            let key = self.parse_class_prop_name()?;
            if readonly.is_some() {
                self.emit_err(span!(self, start), SyntaxError::ReadOnlyMethod);
            }
            if is_constructor(&key) {
                self.emit_err(span!(self, start), SyntaxError::GeneratorConstructor);
            }

            return self.make_method(
                |parser| parser.parse_unique_formal_params(),
                MakeMethodArgs {
                    start,
                    decorators,
                    is_async: false,
                    is_generator: true,
                    is_abstract,
                    is_override,
                    is_optional: false,
                    static_token,
                    key,
                    kind: MethodKind::Method,
                },
            );
        }

        trace_cur!(self, parse_class_member_with_is_static__normal_class_member);
        let key = if readonly.is_some() && is_one_of!(self, '!', ':') {
            Either::Right(PropName::Ident(
                self.new_ident("readonly".into(), readonly.unwrap()),
            ))
        } else {
            self.parse_class_prop_name()?
        };
        let is_optional = self.syntax().typescript() && eat!(self, '?');

        if self.is_class_method()? {
            // handle a(){} / get(){} / set(){} / async(){}

            trace_cur!(self, parse_class_member_with_is_static__normal_class_method);

            match declare_token {
                Some(token) => self.emit_err(token, SyntaxError::TS1031),
                None => {}
            }

            if readonly.is_some() {
                syntax_error!(self, span!(self, start), SyntaxError::ReadOnlyMethod);
            }
            let is_constructor = is_constructor(&key);

            if is_constructor {
                if self.syntax().typescript() && is_override {
                    self.emit_err(
                        span!(self, start),
                        SyntaxError::TS1089(js_word!("override")),
                    );
                }

                if self.syntax().typescript() && is!(self, '<') {
                    let start = self.input.cur_pos();
                    if peeked_is!(self, '>') {
                        self.assert_and_bump(&tok!('<'));
                        let start2 = self.input.cur_pos();
                        self.assert_and_bump(&tok!('>'));

                        self.emit_err(span!(self, start), SyntaxError::TS1098);
                        self.emit_err(span!(self, start2), SyntaxError::TS1092);
                    } else {
                        let type_params = self.try_parse_ts_type_params()?;

                        if let Some(type_params) = type_params {
                            for param in type_params {
                                self.emit_err(param.span(), SyntaxError::TS1092);
                            }
                        }
                    }
                }

                expect!(self, '(');
                // TODO: param props
                let (params, param_props) = self.parse_constructor_params()?;
                expect!(self, ')');

                if self.syntax().typescript() && is!(self, ':') {
                    let start = self.input.cur_pos();
                    let type_ann_span = self.parse_ts_type_ann(true, start)?;

                    self.emit_err(type_ann_span, SyntaxError::TS1093);
                }

                let ctx = Context {
                    span_of_fn_name: Some(key.span()),
                    ..self.ctx()
                };
                let body: Option<_> = self.with_ctx(ctx).parse_fn_body(false, false)?;

                if self.syntax().typescript() && body.is_none() {
                    // Declare constructors cannot have assignment pattern in parameters
                    for p in &params {
                        // TODO(swc): Search deeply for assignment pattern using a Visitor

                        let span = match p.pat {
                            Pat::Assign(ref p) => Some(p.span()),
                            _ => None,
                        };

                        if let Some(span) = span {
                            self.emit_err(span, SyntaxError::TS2371)
                        }
                    }
                }

                if self.syntax().typescript() {
                    if let Some(static_token) = static_token {
                        self.emit_err(static_token, SyntaxError::TS1089(js_word!("static")))
                    }
                }

                if let Some(span) = modifier_span {
                    if is_abstract {
                        self.emit_err(span, SyntaxError::TS1242);
                    }
                }

                return Ok(Some(ClassMember::Constructor(Constructor {
                    node_id: node_id!(self),
                    span: span!(self, start),
                    params,
                    body,
                })));
            } else {
                return self.make_method(
                    |parser| parser.parse_formal_params(),
                    MakeMethodArgs {
                        start,
                        is_optional,
                        decorators,
                        is_abstract,
                        is_override,
                        static_token,
                        kind: MethodKind::Method,
                        key,
                        is_async: false,
                        is_generator: false,
                    },
                );
            }
        }

        if self.is_class_property()? {
            return self.make_property(
                start,
                decorators,
                key,
                is_static,
                is_optional,
                readonly.is_some(),
                declare,
                is_abstract,
                is_override,
            );
        }

        if match key {
            Either::Right(PropName::Ident(ref i)) => i.sym == js_word!("async"),
            _ => false,
        } && !self.input.had_line_break_before_cur()
        {
            // handle async foo(){}

            if self.parse_ts_modifier(&["override"])?.is_some() {
                is_override = true;
                self.emit_err(
                    self.input.prev_span(),
                    SyntaxError::TS1029(js_word!("override"), js_word!("async")),
                );
            }

            let is_generator = eat!(self, '*');
            let key = self.parse_class_prop_name()?;
            if is_constructor(&key) {
                syntax_error!(self, key.span(), SyntaxError::AsyncConstructor)
            }
            if readonly.is_some() {
                syntax_error!(self, span!(self, start), SyntaxError::ReadOnlyMethod);
            }

            // handle async foo(){}
            let is_optional = is_optional || self.syntax().typescript() && eat!(self, '?');
            return self.make_method(
                |parser| parser.parse_unique_formal_params(),
                MakeMethodArgs {
                    start,
                    static_token,
                    key,
                    is_abstract,
                    is_optional,
                    is_override,
                    decorators,
                    kind: MethodKind::Method,
                    is_async: true,
                    is_generator,
                },
            );
        }

        let is_next_line_generator = self.input.had_line_break_before_cur() && is!(self, '*');
        let key_span = key.span();

        match key {
            // `get\n*` is an uninitialized property named 'get' followed by a generator.
            Either::Right(PropName::Ident(ref i))
                if (i.sym == js_word!("get") || i.sym == js_word!("set"))
                    && !is_next_line_generator =>
            {
                // handle get foo(){} / set foo(v){}
                let key = self.parse_class_prop_name()?;

                if readonly.is_some() {
                    self.emit_err(key_span, SyntaxError::GetterSetterCannotBeReadonly);
                }

                return match i.sym {
                    js_word!("get") => self.make_method(
                        |parser| {
                            let params = parser.parse_formal_params()?;

                            // TODO: I think this iterates all of the params.
                            // A short-circuting iter method might be better
                            if params.iter().filter(|param| is_not_this(param)).count() != 0 {
                                parser.emit_err(key_span, SyntaxError::GetterParam);
                            }

                            Ok(params)
                        },
                        MakeMethodArgs {
                            decorators,
                            start,
                            is_abstract,
                            is_async: false,
                            is_generator: false,
                            is_optional,
                            is_override,
                            static_token,
                            key,
                            kind: MethodKind::Getter,
                        },
                    ),
                    js_word!("set") => self.make_method(
                        |parser| {
                            let params = parser.parse_formal_params()?;

                            if params.iter().filter(|param| is_not_this(param)).count() != 1 {
                                parser.emit_err(key_span, SyntaxError::SetterParam);
                            }

                            if !params.is_empty() {
                                if let Pat::Rest(..) = params[0].pat {
                                    parser.emit_err(
                                        params[0].pat.span(),
                                        SyntaxError::RestPatInSetter,
                                    );
                                }
                            }

                            Ok(params)
                        },
                        MakeMethodArgs {
                            decorators,
                            start,
                            is_optional,
                            is_abstract,
                            is_override,
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

    /// Always returns `Some`.
    fn make_property(
        &mut self,
        start: BytePos,
        decorators: Vec<Decorator>,
        key: Either<PrivateName, PropName>,
        is_static: bool,
        is_optional: bool,
        readonly: bool,
        declare: bool,
        is_abstract: bool,
        is_override: bool,
    ) -> PResult<Option<ClassMember>> {
        if is_constructor(&key) {
            syntax_error!(self, key.span(), SyntaxError::PropertyNamedConstructor);
        }
        if declare && key.is_left() {
            syntax_error!(self, key.span(), SyntaxError::DeclarePrivateIdentifier);
        }
        let definite = self.syntax().typescript() && !is_optional && eat!(self, '!');

        let type_ann = self.try_parse_ts_type_ann()?;

        let ctx = Context {
            in_class_prop: true,
            in_method: false,
            include_in_expr: true,
            ..self.ctx()
        };
        self.with_ctx(ctx).parse_with(|parser| {
            let value = if is!(parser, '=') {
                parser.assert_and_bump(&tok!('='));
                Some(parser.parse_assignment_expr()?)
            } else {
                None
            };

            if !eat!(parser, ';') {
                parser.emit_err(parser.input.cur_span(), SyntaxError::TS1005);
            }

            Ok(Some(match key {
                Either::Left(key) => PrivateProp {
                    node_id: node_id!(parser),
                    span: span!(parser, start),
                    key,
                    value,
                    is_static,
                    decorators,
                }
                .into(),
                Either::Right(key) => ClassProp {
                    node_id: node_id!(parser),
                    span: span!(parser, start),
                    key,
                    value,
                    is_static,
                    decorators,
                }
                .into(),
            }))
        })
    }

    fn is_class_method(&mut self) -> PResult<bool> {
        Ok(is!(self, '(')
            || (self.syntax().typescript() && is!(self, '<'))
            || (self.syntax().typescript() && is!(self, JSXTagStart)))
    }

    fn is_class_property(&mut self) -> PResult<bool> {
        Ok((self.syntax().typescript() && is_one_of!(self, '!', ':'))
            || is_one_of!(self, '=', ';', '}'))
    }

    fn parse_fn<T>(
        &mut self,
        start_of_output_type: Option<BytePos>,
        start_of_async: Option<BytePos>,
        decorators: Vec<Decorator>,
    ) -> PResult<T>
    where
        T: OutputType,
        Self: MaybeOptionalIdentParser<T::Ident>,
        T::Ident: Spanned,
    {
        let start = start_of_async.unwrap_or_else(|| self.input.cur_pos());
        self.assert_and_bump(&tok!("function"));
        let is_async = start_of_async.is_some();

        let is_generator = {
            if self.input.eat(&tok!('*')) {
                // if is_async {
                //     syntax_error!(self, span!(self, start), SyntaxError::AsyncGenerator {});
                // }
                true
            } else {
                false
            }
        };

        let ctx = Context {
            in_async: is_async,
            in_generator: is_generator,
            ..self.ctx()
        };

        let ident = if T::is_fn_expr() {
            //
            self.with_ctx(Context {
                in_generator: is_generator,
                ..ctx
            })
            .parse_maybe_opt_binding_ident()?
        } else {
            // function declaration does not change context for `BindingIdentifier`.
            self.parse_maybe_opt_binding_ident()?
        };
        let ctx = Context {
            span_of_fn_name: Some(ident.span()),
            ..ctx
        };

        self.with_ctx(ctx).parse_with(|parser| {
            let f = parser.parse_fn_args_body(
                decorators,
                start,
                |parser| parser.parse_formal_params(),
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

            Ok(T::finish_fn(
                span!(parser, start_of_output_type.unwrap_or(start)),
                ident,
                f,
                node_id!(parser),
            ))
        })
    }

    /// `parse_args` closure should not eat '(' or ')'.
    pub(super) fn parse_fn_args_body<F>(
        &mut self,
        decorators: Vec<Decorator>,
        start: BytePos,
        parse_args: F,
        is_async: bool,
        is_generator: bool,
    ) -> PResult<Function>
    where
        F: FnOnce(&mut Self) -> PResult<Vec<Param>>,
    {
        trace_cur!(self, parse_fn_args_body);

        // let prev_in_generator = self.ctx().in_generator;
        let ctx = Context {
            in_async: is_async,
            in_generator: is_generator,
            ..self.ctx()
        };

        self.with_ctx(ctx).parse_with(|parser| {
            let type_params = if parser.syntax().typescript() {
                parser.in_type().parse_with(|parser| {
                    trace_cur!(p, parse_fn_args_body__type_params);

                    Ok(if is!(parser, '<') {
                        Some(parser.parse_ts_type_params()?)
                    } else if is!(parser, JSXTagStart) {
                        debug_assert_eq!(
                            parser.input.token_context().current(),
                            Some(TokenContext::JSXOpeningTag)
                        );
                        parser.input.token_context_mut().pop();
                        debug_assert_eq!(
                            parser.input.token_context().current(),
                            Some(TokenContext::JSXExpr)
                        );
                        parser.input.token_context_mut().pop();

                        Some(parser.parse_ts_type_params()?)
                    } else {
                        None
                    })
                })?
            } else {
                None
            };

            expect!(parser, '(');

            let arg_ctx = Context {
                in_parameters: true,
                // in_generator: prev_in_generator,
                ..parser.ctx()
            };
            let params = parser
                .with_ctx(arg_ctx)
                .parse_with(|parser| parse_args(parser))?;

            expect!(parser, ')');

            // typescript extension
            let return_type = if parser.syntax().typescript() && is!(parser, ':') {
                parser
                    .parse_ts_type_or_type_predicate_ann(&tok!(':'))
                    .map(Some)?
            } else {
                None
            };

            let body: Option<_> = parser.parse_fn_body(is_async, is_generator)?;

            if parser.syntax().typescript() && body.is_none() {
                // Declare functions cannot have assignment pattern in parameters
                for param in &params {
                    // TODO(swc): Search deeply for assignment pattern using a Visitor

                    let span = match &param.pat {
                        Pat::Assign(ref p) => Some(p.span()),
                        _ => None,
                    };

                    if let Some(span) = span {
                        parser.emit_err(span, SyntaxError::TS2371)
                    }
                }
            }

            Ok(Function {
                node_id: node_id!(parser),
                span: span!(parser, start),
                decorators,
                params,
                body,
                is_async,
                is_generator,
            })
        })
    }

    fn parse_class_prop_name(&mut self) -> PResult<Either<PrivateName, PropName>> {
        if is!(self, '#') {
            self.parse_private_name().map(Either::Left)
        } else {
            self.parse_prop_name().map(Either::Right)
        }
    }

    pub(super) fn parse_fn_body<T>(&mut self, is_async: bool, is_generator: bool) -> PResult<T>
    where
        Self: FnBodyParser<T>,
    {
        if self.ctx().in_declare && self.syntax().typescript() && is!(self, '{') {
            //            self.emit_err(
            //                self.ctx().span_of_fn_name.expect("we are not in function"),
            //                SyntaxError::TS1183,
            //            );
            self.emit_err(self.input.cur_span(), SyntaxError::TS1183);
        }

        let ctx = Context {
            in_async: is_async,
            in_generator: is_generator,
            in_function: true,
            is_break_allowed: false,
            is_continue_allowed: false,
            ..self.ctx()
        };
        let state = State {
            labels: vec![],
            ..Default::default()
        };
        self.with_ctx(ctx).with_state(state).parse_fn_body_inner()
    }
}

impl<I: Tokens> Parser<I> {
    /// Always returns `Some`.
    fn make_method<F>(
        &mut self,
        parse_args: F,
        MakeMethodArgs {
            start,
            is_abstract,
            static_token,
            decorators,
            is_optional,
            is_override,
            key,
            kind,
            is_async,
            is_generator,
        }: MakeMethodArgs,
    ) -> PResult<Option<ClassMember>>
    where
        F: FnOnce(&mut Self) -> PResult<Vec<Param>>,
    {
        trace_cur!(self, make_method);

        let is_static = static_token.is_some();
        let ctx = Context {
            span_of_fn_name: Some(key.span()),
            ..self.ctx()
        };
        let function = self.with_ctx(ctx).parse_with(|parser| {
            parser.parse_fn_args_body(decorators, start, parse_args, is_async, is_generator)
        })?;

        match kind {
            MethodKind::Getter | MethodKind::Setter
                if self.syntax().typescript() && self.input.target() == JscTarget::Es3 =>
            {
                self.emit_err(key.span(), SyntaxError::TS1056);
            }
            _ => {}
        }

        Ok(Some(match key {
            Either::Left(key) => PrivateMethod {
                node_id: node_id!(self),
                span: span!(self, start),

                is_static,
                key,
                function,
                kind,
            }
            .into(),
            Either::Right(key) => ClassMethod {
                node_id: node_id!(self),
                span: span!(self, start),

                is_static,
                key,
                function,
                kind,
            }
            .into(),
        }))
    }
}

trait IsInvalidClassName {
    fn invalid_class_name(&self) -> Option<Span>;
}

impl IsInvalidClassName for Ident {
    fn invalid_class_name(&self) -> Option<Span> {
        match self.sym {
            js_word!("any") => Some(self.span),
            _ => None,
        }
    }
}
impl IsInvalidClassName for Option<Ident> {
    fn invalid_class_name(&self) -> Option<Span> {
        if let Some(i) = self.as_ref() {
            return i.invalid_class_name();
        }

        None
    }
}

trait OutputType {
    type Ident: IsInvalidClassName;

    fn is_constructor(ident: &Self::Ident) -> bool;

    /// From babel..
    ///
    /// When parsing function expression, the binding identifier is parsed
    /// according to the rules inside the function.
    /// e.g. (function* yield() {}) is invalid because "yield" is disallowed in
    /// generators.
    /// This isn't the case with function declarations: function* yield() {} is
    /// valid because yield is parsed as if it was outside the generator.
    /// Therefore, this.state.inGenerator is set before or after parsing the
    /// function id according to the "isStatement" parameter.
    fn is_fn_expr() -> bool {
        false
    }

    fn finish_fn(span: Span, ident: Self::Ident, f: Function, node_id: NodeId) -> Self;
    fn finish_class(span: Span, ident: Self::Ident, class: Class, node_id: NodeId) -> Self;
}

impl OutputType for Box<Expr> {
    type Ident = Option<Ident>;

    fn is_constructor(ident: &Self::Ident) -> bool {
        match *ident {
            Some(ref i) => i.sym == js_word!("constructor"),
            _ => false,
        }
    }

    fn is_fn_expr() -> bool {
        true
    }

    fn finish_fn(_: Span, ident: Option<Ident>, function: Function, node_id: NodeId) -> Self {
        Box::new(Expr::Fn(FnExpr {
            ident,
            function,
            node_id,
        }))
    }
    fn finish_class(_: Span, ident: Option<Ident>, class: Class, node_id: NodeId) -> Self {
        Box::new(Expr::Class(ClassExpr {
            ident,
            class,
            node_id,
        }))
    }
}

impl OutputType for ExportDefaultDecl {
    type Ident = Option<Ident>;

    fn is_constructor(ident: &Self::Ident) -> bool {
        match *ident {
            Some(ref i) => i.sym == js_word!("constructor"),
            _ => false,
        }
    }

    fn finish_fn(span: Span, ident: Option<Ident>, function: Function, node_id: NodeId) -> Self {
        ExportDefaultDecl {
            span,
            decl: DefaultDecl::Fn(FnExpr {
                ident,
                function,
                node_id,
            }),
            node_id,
        }
    }
    fn finish_class(span: Span, ident: Option<Ident>, class: Class, node_id: NodeId) -> Self {
        ExportDefaultDecl {
            span,
            decl: DefaultDecl::Class(ClassExpr {
                ident,
                class,
                node_id,
            }),
            node_id,
        }
    }
}

impl OutputType for Decl {
    type Ident = Ident;

    fn is_constructor(i: &Self::Ident) -> bool {
        i.sym == js_word!("constructor")
    }

    fn finish_fn(_: Span, ident: Ident, function: Function, node_id: NodeId) -> Self {
        Decl::Fn(FnDecl {
            ident,
            function,
            node_id,
        })
    }
    fn finish_class(_: Span, ident: Ident, class: Class, node_id: NodeId) -> Self {
        Decl::Class(ClassDecl {
            ident,
            class,
            node_id,
        })
    }
}

pub(super) trait FnBodyParser<Body> {
    fn parse_fn_body_inner(&mut self) -> PResult<Body>;
}

impl<I: Tokens> FnBodyParser<BlockStmtOrExpr> for Parser<I> {
    fn parse_fn_body_inner(&mut self) -> PResult<BlockStmtOrExpr> {
        if self.input.is(&tok!('{')) {
            self.parse_block(false).map(BlockStmtOrExpr::BlockStmt)
        } else {
            self.parse_assignment_expr().map(BlockStmtOrExpr::Expr)
        }
    }
}

impl<I: Tokens> FnBodyParser<Option<BlockStmt>> for Parser<I> {
    fn parse_fn_body_inner(&mut self) -> PResult<Option<BlockStmt>> {
        // allow omitting body and allow placing `{` on next line
        if self.input.syntax().typescript() && !is!(self, '{') && eat!(self, ';') {
            return Ok(None);
        }
        self.include_in_expr(true).parse_block(true).map(Some)
    }
}

fn is_constructor(key: &Either<PrivateName, PropName>) -> bool {
    matches!(
        *key,
        Either::Right(PropName::Ident(Ident {
            sym: js_word!("constructor"),
            ..
        })) | Either::Right(PropName::Str(Str {
            value: js_word!("constructor"),
            ..
        }))
    )
}

pub(crate) fn is_not_this(param: &Param) -> bool {
    !matches!(
        param.pat,
        Pat::Ident(BindingIdent {
            id: Ident {
                sym: js_word!("this"),
                ..
            },
            ..
        })
    )
}

struct MakeMethodArgs {
    start: BytePos,
    is_abstract: bool,
    static_token: Option<Span>,
    decorators: Vec<Decorator>,
    is_optional: bool,
    is_override: bool,
    key: Either<PrivateName, PropName>,
    kind: MethodKind,
    is_async: bool,
    is_generator: bool,
}
