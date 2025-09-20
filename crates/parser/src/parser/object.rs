//! Parser for object literal.

use crate::context::ContextFlags;

use super::{class_and_fn::is_not_this, util::ParseObject, *};
use atoms::js_word;
use util::AssignProps;

impl<I: Tokens> Parser<'_, I> {
    /// Parse a object literal or object pattern.
    pub(super) fn parse_object<T>(&mut self, assign_props: &mut AssignProps) -> PResult<T>
    where
        Self: ParseObject<T>,
    {
        trace_cur!(self, parse_object);

        let start = self.input.cur_pos();
        self.assert_and_bump(&tok!('{'));

        let mut props = vec![];

        let mut first = true;
        while !self.input.eat(&tok!('}')) {
            // Handle comma
            if first {
                first = false;
            } else {
                expect!(self, ',');
                if self.input.eat(&tok!('}')) {
                    break;
                }
            }

            let prop = self.parse_object_prop(assign_props)?;
            props.push(prop);
        }

        self.make_object(span!(self, start), props)
    }

    /// spec: 'PropertyName'
    pub(super) fn parse_prop_name(&mut self) -> PResult<PropName> {
        self.with_ctx(Context {
            flags: self.ctx().flags | ContextFlags::in_property_name,
            ..self.ctx()
        })
        .parse_with(|parser| {
            let start = parser.input.cur_pos();

            let v = match *cur!(parser, true)? {
                Token::Str { .. } => match parser.input.bump() {
                    Token::Str { value, has_escape } => PropName::Str(Str {
                        node_id: node_id!(parser, span!(parser, start)),
                        value,
                        has_escape,
                    }),
                    _ => unreachable!(),
                },
                Token::Num { .. } => match parser.input.bump() {
                    Token::Num(value) => PropName::Num(Number {
                        node_id: node_id!(parser, span!(parser, start)),
                        value,
                    }),
                    _ => unreachable!(),
                },
                Token::BigInt(_) => match parser.input.bump() {
                    Token::BigInt(value) => PropName::BigInt(BigInt {
                        node_id: node_id!(parser, span!(parser, start)),
                        value,
                    }),
                    _ => unreachable!(),
                },
                Word(..) => match parser.input.bump() {
                    Word(w) => PropName::Ident(parser.new_ident(w.into(), span!(parser, start))),
                    _ => unreachable!(),
                },
                tok!('[') => {
                    parser.input.bump();
                    let inner_start = parser.input.cur_pos();

                    let mut expr = parser
                        .include_in_expr(true)
                        .parse_assignment_expr(&mut AssignProps::Emit)?
                        .unwrap();

                    if parser.syntax().typescript() && is!(parser, ',') {
                        let mut exprs = vec![expr];

                        while eat!(parser, ',') {
                            exprs.push(
                                parser
                                    .include_in_expr(true)
                                    .parse_assignment_expr(&mut AssignProps::Emit)?
                                    .unwrap(),
                            );
                        }

                        parser.emit_err(span!(parser, inner_start), SyntaxError::TS1171);

                        expr = Box::new(Expr::Seq(SeqExpr {
                            node_id: node_id!(parser, span!(parser, inner_start)),
                            exprs,
                        }));
                    }

                    expect!(parser, ']');

                    PropName::Computed(ComputedPropName {
                        node_id: node_id!(parser, span!(parser, start)),
                        expr,
                    })
                }
                _ => unexpected!(
                    parser,
                    "identifier, string literal, numeric literal or [ for the computed key"
                ),
            };

            Ok(v)
        })
    }
}

impl<I: Tokens> ParseObject<Box<Expr>> for Parser<'_, I> {
    type Prop = Prop;

    fn make_object(&mut self, span: Span, props: Vec<Self::Prop>) -> PResult<Box<Expr>> {
        Ok(Box::new(Expr::Object(ObjectLit {
            node_id: node_id!(self, span),
            props,
        })))
    }

    /// spec: 'PropertyDefinition'
    fn parse_object_prop(&mut self, assign_props: &mut AssignProps) -> PResult<Self::Prop> {
        trace_cur!(self, parse_object_prop);

        let start = self.input.cur_pos();
        // Parse as 'MethodDefinition'

        if self.input.eat(&tok!("...")) {
            // spread element

            let expr = self
                .include_in_expr(true)
                .parse_assignment_expr(assign_props)?
                .unwrap();

            let span = Span::new(start, self.input.last_pos());
            return Ok(Prop::Spread(SpreadAssignment {
                node_id: node_id!(self, span),
                expr,
            }));
        }

        if self.input.eat(&tok!('*')) {
            let name = self.parse_prop_name()?;
            return self
                .parse_fn_args_body(start, Parser::parse_unique_formal_params, false, true)
                .map(|function| {
                    Prop::Method(MethodProp {
                        node_id: node_id_from!(self, function.node_id),
                        key: name,
                        function,
                    })
                });
        }

        let has_modifiers = self.eat_any_ts_modifier()?;
        let modifiers_span = self.input.prev_span();

        let key_start = self.input.cur_pos();
        let key = self.parse_prop_name()?;

        if self.input.syntax().typescript()
            && !is_one_of!(self, '(', '[', ':', ',', '?', '=', '*', IdentName, Str, Num)
            && !(self.input.syntax().typescript() && is!(self, '<'))
            && !(is!(self, '}') && matches!(key, PropName::Ident(..)))
        {
            trace_cur!(self, parse_object_prop_error);

            self.emit_err(self.input.cur_span(), SyntaxError::TS1005);
            let span = Span::new(key_start, self.input.cur_pos());
            return Ok(Prop::KeyValue(KeyValueProp {
                node_id: node_id!(self, span),
                key,
                value: Box::new(Expr::Invalid(Invalid {
                    node_id: node_id!(self, span!(self, start)),
                })),
            }));
        }
        //
        // {[computed()]: a,}
        // { 'a': a, }
        // { 0: 1, }
        // { a: expr, }
        if self.input.eat(&tok!(':')) {
            let value = self
                .include_in_expr(true)
                .parse_assignment_expr(assign_props)?
                .unwrap();
            let span = Span::new(key_start, self.input.last_pos());
            return Ok(Prop::KeyValue(KeyValueProp {
                node_id: node_id!(self, span),
                key,
                value,
            }));
        }

        // Handle `a(){}` (and async(){} / get(){} / set(){})
        if (self.input.syntax().typescript() && is!(self, '<')) || is!(self, '(') {
            return self
                .parse_fn_args_body(start, Parser::parse_unique_formal_params, false, false)
                .map(|function| {
                    Prop::Method(MethodProp {
                        node_id: node_id_from!(self, function.node_id),
                        key,
                        function,
                    })
                });
        }

        let ident = match key {
            PropName::Ident(ident) => ident,
            // TODO
            _ => unexpected!(self, "identifier"),
        };

        if self.input.eat(&tok!('?')) {
            self.emit_err(self.input.prev_span(), SyntaxError::TS1162);
        }

        // `ident` from parse_prop_name is parsed as 'IdentifierName'
        // It means we should check for invalid expressions like { for, }
        if is_one_of!(self, '=', ',', '}') {
            if self.ctx().is_reserved_word(&ident.sym) {
                self.emit_err(
                    get_span!(self, ident.node_id),
                    SyntaxError::ReservedWordInObjShorthandOrPat,
                );
            }

            if self.input.eat(&tok!('=')) {
                let value = self
                    .include_in_expr(true)
                    .parse_assignment_expr(assign_props)?
                    .unwrap();
                let span = Span::new(key_start, self.input.last_pos());
                match assign_props {
                    AssignProps::Buffer(buffer) => {
                        buffer.push(span);
                    }
                    AssignProps::Emit => {
                        self.emit_err(span, SyntaxError::AssignProperty);
                    }
                    AssignProps::Ignore => {}
                }
                return Ok(Prop::Assign(AssignProp {
                    node_id: node_id!(self, span),
                    key: ident,
                    value,
                }));
            }

            let span = Span::new(key_start, self.input.last_pos());
            return Ok(Prop::KeyValue(KeyValueProp {
                node_id: node_id!(self, span),
                value: Box::new(Expr::Ident(ident.clone_node(program_data!(self)))),
                key: PropName::Ident(ident),
            }));
        }

        // get a(){}
        // set a(v){}
        // async a(){}

        match ident.sym {
            js_word!("get") | js_word!("set") | js_word!("async") => {
                trace_cur!(self, parse_object_prop__after_accessor);

                if has_modifiers {
                    self.emit_err(modifiers_span, SyntaxError::TS1042);
                }

                let is_generator = ident.sym == js_word!("async") && self.input.eat(&tok!('*'));
                let key = self.parse_prop_name()?;
                let key_span = get_span!(self, key.node_id());

                match ident.sym {
                    js_word!("get") => self
                        .parse_fn_args_body(
                            start,
                            |parser| {
                                let params = parser.parse_formal_params()?;

                                // TODO: I think this iterates all of the params.
                                // A short-circuting iter method might be better
                                if params.iter().filter(|p| is_not_this(p)).count() != 0 {
                                    parser.emit_err(key_span, SyntaxError::GetterParam);
                                }

                                Ok(params)
                            },
                            false,
                            false,
                        )
                        .map(|Function { body, .. }| {
                            if self.input.syntax().typescript()
                                && self.input.target() == JscTarget::Es3
                            {
                                self.emit_err(key_span, SyntaxError::TS1056);
                            }

                            Prop::Getter(GetterProp {
                                node_id: node_id!(self, span!(self, start)),
                                key,
                                body,
                            })
                        }),
                    js_word!("set") => self
                        .parse_fn_args_body(
                            start,
                            |parser| {
                                let params = parser.parse_formal_params()?;

                                if params.iter().filter(|p| is_not_this(p)).count() != 1 {
                                    parser.emit_err(key_span, SyntaxError::SetterParam);
                                }

                                if !params.is_empty() {
                                    if let Pat::Rest(first) = &params[0].pat {
                                        parser.emit_err(
                                            get_span!(parser, first.node_id),
                                            SyntaxError::RestPatInSetter,
                                        );
                                    }
                                }

                                if parser.input.syntax().typescript()
                                    && parser.input.target() == JscTarget::Es3
                                {
                                    parser.emit_err(key_span, SyntaxError::TS1056);
                                }

                                Ok(params)
                            },
                            false,
                            false,
                        )
                        .map(|Function { params, body, .. }| {
                            // debug_assert_eq!(params.len(), 1);
                            Prop::Setter(SetterProp {
                                node_id: node_id!(self, span!(self, start)),
                                key,
                                body,
                                param: params.into_iter().next().unwrap_or_else(|| {
                                    Param::from_pat(
                                        Pat::Invalid(Invalid {
                                            node_id: node_id!(self, key_span),
                                        }),
                                        program_data!(self),
                                    )
                                }),
                            })
                        }),
                    js_word!("async") => self
                        .parse_fn_args_body(
                            start,
                            Parser::parse_unique_formal_params,
                            true,
                            is_generator,
                        )
                        .map(|function| {
                            Prop::Method(MethodProp {
                                node_id: node_id_from!(self, function.node_id),
                                key,
                                function,
                            })
                        }),
                    _ => unreachable!(),
                }
            }
            _ => {
                if self.input.syntax().typescript() {
                    unexpected!(
                        self,
                        "... , *,  (, [, :, , ?, =, an identifier, public, protected, private, \
                         readonly, <."
                    )
                } else {
                    unexpected!(self, "... , *,  (, [, :, , ?, = or an identifier")
                }
            }
        }
    }
}

impl<I: Tokens> ParseObject<Pat> for Parser<'_, I> {
    type Prop = ObjectPatProp;

    fn make_object(&mut self, span: Span, props: Vec<Self::Prop>) -> PResult<Pat> {
        let len = props.len();
        for (i, p) in props.iter().enumerate() {
            if i == len - 1 {
                if let ObjectPatProp::Rest(rest) = p {
                    match *rest.arg {
                        Pat::Ident(..) => {}
                        _ => syntax_error!(
                            self,
                            get_span!(self, rest.node_id),
                            SyntaxError::DotsWithoutIdentifier
                        ),
                    }
                }
                continue;
            }

            if let ObjectPatProp::Rest(p) = p {
                if self.syntax().early_errors() {
                    syntax_error!(
                        self,
                        get_span!(self, p.node_id),
                        SyntaxError::NonLastRestParam
                    )
                }
            }
        }

        // TS optional.
        if self.input.syntax().dts() || self.ctx().in_declare() {
            self.input.eat(&tok!('?'));
        }

        Ok(Pat::Object(ObjectPat {
            node_id: node_id!(self, span),
            props,
        }))
    }

    /// Production 'BindingProperty'
    fn parse_object_prop(&mut self, _assign_props: &mut AssignProps) -> PResult<Self::Prop> {
        let start = self.input.cur_pos();

        if self.input.eat(&tok!("...")) {
            // spread element

            let arg = Box::new(self.parse_binding_pat_or_ident()?);

            return Ok(ObjectPatProp::Rest(RestPat {
                node_id: node_id!(self, span!(self, start)),
                arg,
            }));
        }

        let key_start = self.input.cur_pos();
        let key = self.parse_prop_name()?;
        let key_span = Span::new(key_start, self.input.last_pos());
        if self.input.eat(&tok!(':')) {
            let value = Box::new(self.parse_binding_element()?);

            let span = Span::new(key_start, self.input.last_pos());
            return Ok(ObjectPatProp::KeyValue(KeyValuePatProp {
                node_id: node_id!(self, span),
                key,
                value,
            }));
        }
        let key = match key {
            PropName::Ident(ident) => ident,
            _ => unexpected!(self, "an identifier"),
        };

        let value = if self.input.eat(&tok!('=')) {
            self.include_in_expr(true)
                .parse_assignment_expr(&mut AssignProps::Emit)
                .map(Some)?
        } else {
            if self.ctx().is_reserved_word(&key.sym) {
                self.emit_err(
                    get_span!(self, key.node_id),
                    SyntaxError::ReservedWordInObjShorthandOrPat,
                );
            }

            None
        };

        if let Some(value) = value {
            let pat_span = Span::new(key_start, self.input.last_pos());
            let assign_pat = AssignPat {
                node_id: node_id!(self, pat_span),
                left: Box::new(Pat::Ident(BindingIdent {
                    node_id: node_id!(self, key_span),
                    id: key.clone_node(program_data!(self)),
                })),
                right: value.unwrap(),
            };
            Ok(ObjectPatProp::KeyValue(KeyValuePatProp {
                node_id: node_id!(self, pat_span),
                key: PropName::Ident(key.clone_node(program_data!(self))),
                value: Box::new(Pat::Assign(assign_pat)),
            }))
        } else {
            Ok(ObjectPatProp::KeyValue(KeyValuePatProp {
                node_id: node_id!(self, key_span),
                value: Box::new(Pat::Ident(BindingIdent {
                    node_id: node_id!(self, key_span),
                    id: key.clone_node(program_data!(self)),
                })),
                key: PropName::Ident(key),
            }))
        }
    }
}
