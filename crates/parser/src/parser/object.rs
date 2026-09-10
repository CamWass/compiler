//! Parser for object literal.

use crate::context::ContextFlags;

use super::*;
use util::AssignProps;

impl Parser<'_> {
    pub(super) fn parse_object_lit(
        &mut self,
        assign_props: &mut AssignProps,
    ) -> PResult<Box<Expr>> {
        let start = self.input.cur_pos();
        self.assert_and_bump(tok!('{'));

        let mut props = vec![];

        let mut first = true;
        while !self.eat(tok!('}')) {
            // Handle comma
            if first {
                first = false;
            } else {
                expect!(self, ',');
                if self.eat(tok!('}')) {
                    break;
                }
            }

            let prop = self.parse_object_lit_prop(assign_props)?;
            props.push(prop);
        }

        Ok(Box::new(Expr::Object(ObjectLit {
            node_id: node_id!(self, self.span(start)),
            props,
        })))
    }

    /// spec: 'PropertyDefinition'
    fn parse_object_lit_prop(&mut self, assign_props: &mut AssignProps) -> PResult<Prop> {
        let start = self.input.cur_pos();
        // Parse as 'MethodDefinition'

        if self.eat(tok!("...")) {
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

        if self.eat(tok!('*')) {
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

        let has_modifiers = self.eat_any_ts_modifier();
        let modifiers_span = self.input.prev_span();

        let key_start = self.input.cur_pos();
        let key = self.parse_prop_name()?;

        if self.input.syntax().typescript()
            && !(self.is(tok!('('))
                || self.is(tok!('['))
                || self.is(tok!(':'))
                || self.is(tok!(','))
                || self.is(tok!('?'))
                || self.is(tok!('='))
                || self.is(tok!('*'))
                || matches!(self.input.cur(), Token::Num | Token::Str)
                || self.input.cur().is_word())
            && !(self.input.syntax().typescript() && self.is(tok!('<')))
            && !(self.is(tok!('}')) && matches!(key, PropName::Ident(..)))
        {
            self.emit_err(self.input.cur_span(), SyntaxError::TS1005);
            let span = Span::new(key_start, self.input.cur_pos());
            return Ok(Prop::KeyValue(KeyValueProp {
                node_id: node_id!(self, span),
                key,
                value: Box::new(self.create_invalid_expr()),
            }));
        }
        //
        // {[computed()]: a,}
        // { 'a': a, }
        // { 0: 1, }
        // { a: expr, }
        if self.eat(tok!(':')) {
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
        if (self.input.syntax().typescript() && self.is(tok!('<'))) || self.is(tok!('(')) {
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

        let PropName::Ident(ident) = key else {
            // TODO
            unexpected!(self, "identifier")
        };

        if self.eat(tok!('?')) {
            self.emit_err(self.input.prev_span(), SyntaxError::TS1162);
        }

        // `ident` from parse_prop_name is parsed as 'IdentifierName'
        // It means we should check for invalid expressions like { for, }
        if self.is(tok!('=')) || self.is(tok!(',')) || self.is(tok!('}')) {
            if self.ctx().is_reserved_word(ident.name) {
                self.emit_err(
                    get_span!(self, ident.node_id),
                    SyntaxError::ReservedWordInObjShorthandOrPat,
                );
            }

            if self.eat(tok!('=')) {
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
                value: Box::new(Expr::Ident(ident.clone_node(program_data!(self).data()))),
                key: PropName::Ident(ident),
            }));
        }

        // get a(){}
        // set a(v){}
        // async a(){}

        match ident.name {
            id_for_built_in!("get") | id_for_built_in!("set") | id_for_built_in!("async") => {
                if has_modifiers {
                    self.emit_err(modifiers_span, SyntaxError::TS1042);
                }

                let is_generator = ident.name == id_for_built_in!("async") && self.eat(tok!('*'));
                let key = self.parse_prop_name()?;
                let key_span = get_span!(self, key.node_id());

                match ident.name {
                    id_for_built_in!("get") => self
                        .parse_fn_args_body(start, Parser::parse_formal_params, false, false)
                        .map(|Function { body, params, .. }| {
                            for param in &params.params {
                                self.emit_err(
                                    get_span!(self, param.node_id),
                                    SyntaxError::GetterParam,
                                );
                            }

                            if let Some(rest) = params.rest_param {
                                self.emit_err(
                                    get_span!(self, rest.node_id),
                                    SyntaxError::GetterParam,
                                );
                            }

                            if self.input.syntax().typescript()
                                && self.input.target() == JscTarget::Es3
                            {
                                self.emit_err(key_span, SyntaxError::TS1056);
                            }

                            Prop::Getter(GetterProp {
                                node_id: node_id!(self, self.span(start)),
                                key,
                                body,
                            })
                        }),
                    id_for_built_in!("set") => self
                        .parse_fn_args_body(start, Parser::parse_formal_params, false, false)
                        .map(|Function { params, body, .. }| {
                            if let Some(rest_param) = &params.rest_param {
                                self.emit_err(
                                    get_span!(self, rest_param.node_id),
                                    SyntaxError::RestPatInSetter,
                                );
                            } else if params.params.len() != 1 {
                                self.emit_err(
                                    get_span!(self, key.node_id()),
                                    SyntaxError::SetterParam,
                                );
                            }

                            if self.input.syntax().typescript()
                                && self.input.target() == JscTarget::Es3
                            {
                                self.emit_err(key_span, SyntaxError::TS1056);
                            }

                            Prop::Setter(SetterProp {
                                node_id: node_id!(self, self.span(start)),
                                key,
                                body,
                                param: params
                                    .params
                                    .into_iter()
                                    .next()
                                    .unwrap_or_else(|| self.create_invalid_param()),
                            })
                        }),
                    id_for_built_in!("async") => self
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

    pub(super) fn parse_object_binding_pat(&mut self) -> PResult<ObjectBindingPat> {
        let start = self.input.cur_pos();
        self.assert_and_bump(tok!('{'));

        let mut props = vec![];
        let mut rest = None;

        let mut first = true;
        while !self.eat(tok!('}')) {
            // Handle comma
            if first {
                first = false;
            } else {
                expect!(self, ',');
                if self.eat(tok!('}')) {
                    break;
                }
            }

            let start = self.input.cur_pos();

            if self.eat(tok!("...")) {
                // spread element

                let arg = Box::new(BindingIdent::from_ident(self.parse_binding_ident()?));

                rest = Some(BindingRestProperty {
                    node_id: node_id!(self, self.span(start)),
                    arg,
                });
                expect!(self, '}');
                break;
            } else {
                let prop = self.parse_object_pat_prop()?;
                props.push(prop);
            }
        }

        let span = self.span(start);

        // TS optional.
        if self.input.syntax().dts() || self.ctx().in_declare() {
            self.eat(tok!('?'));
        }

        Ok(ObjectBindingPat {
            node_id: node_id!(self, span),
            props,
            rest,
        })
    }

    fn parse_object_pat_prop(&mut self) -> PResult<BindingProperty> {
        let key_start = self.input.cur_pos();
        let prop = self.parse_prop_name()?;
        let key_span = Span::new(key_start, self.input.last_pos());
        if self.eat(tok!(':')) {
            let target = Box::new(self.parse_binding_element()?);

            let span = Span::new(key_start, self.input.last_pos());
            return Ok(BindingProperty {
                node_id: node_id!(self, span),
                prop,
                target,
            });
        }
        let PropName::Ident(prop) = prop else {
            unexpected!(self, "an identifier")
        };

        let value = if self.eat(tok!('=')) {
            self.include_in_expr(true)
                .parse_assignment_expr(&mut AssignProps::Emit)
                .map(Some)?
        } else {
            if self.ctx().is_reserved_word(prop.name) {
                self.emit_err(
                    get_span!(self, prop.node_id),
                    SyntaxError::ReservedWordInObjShorthandOrPat,
                );
            }

            None
        };

        if let Some(value) = value {
            let pat_span = Span::new(key_start, self.input.last_pos());
            Ok(BindingProperty {
                node_id: node_id!(self, pat_span),
                prop: PropName::Ident(prop.clone_node(program_data!(self).data())),
                target: Box::new(BindingElement {
                    node_id: node_id!(self, pat_span),
                    target: BindingPatOrIdent::Ident(BindingIdent {
                        id: prop.clone_node(program_data!(self).data()),
                    }),
                    init: Some(value.unwrap()),
                }),
            })
        } else {
            Ok(BindingProperty {
                node_id: node_id!(self, key_span),
                target: Box::new(BindingElement {
                    node_id: node_id!(self, key_span),
                    target: BindingPatOrIdent::Ident(BindingIdent {
                        id: prop.clone_node(program_data!(self).data()),
                    }),
                    init: None,
                }),
                prop: PropName::Ident(prop),
            })
        }
    }

    /// spec: 'PropertyName'
    pub(super) fn parse_prop_name(&mut self) -> PResult<PropName> {
        self.with_ctx(Context {
            flags: self.ctx().flags | ContextFlags::in_property_name,
            ..self.ctx()
        })
        .parse_with(|parser| {
            let start = parser.input.cur_pos();

            let v = match parser.input.cur() {
                Token::Str => {
                    let value = parser.input.expect_str_token_and_bump();
                    PropName::Str(Str {
                        node_id: node_id!(parser, parser.span(start)),
                        value,
                    })
                }
                Token::Num => {
                    let value = parser.input.expect_num_token_and_bump();
                    PropName::Num(Number {
                        node_id: node_id!(parser, parser.span(start)),
                        value,
                    })
                }
                Token::BigInt => {
                    let value = parser.input.expect_big_int_token_and_bump();
                    PropName::BigInt(BigInt {
                        node_id: node_id!(parser, parser.span(start)),
                        value,
                    })
                }
                t if t.is_word() => {
                    let w = parser.input.expect_word_token_and_bump();

                    PropName::Ident(parser.new_ident(w, parser.span(start)))
                }
                tok!('[') => {
                    parser.input.bump();
                    let inner_start = parser.input.cur_pos();

                    let mut expr = parser
                        .include_in_expr(true)
                        .parse_assignment_expr(&mut AssignProps::Emit)?
                        .unwrap();

                    if parser.syntax().typescript() && parser.is(tok!(',')) {
                        let mut exprs = vec![*expr];

                        while parser.eat(tok!(',')) {
                            exprs.push(
                                *parser
                                    .include_in_expr(true)
                                    .parse_assignment_expr(&mut AssignProps::Emit)?
                                    .unwrap(),
                            );
                        }

                        parser.emit_err(parser.span(inner_start), SyntaxError::TS1171);

                        expr = Box::new(Expr::Seq(SeqExpr {
                            node_id: node_id!(parser, parser.span(inner_start)),
                            exprs,
                        }));
                    }

                    expect!(parser, ']');

                    PropName::Computed(ComputedPropName {
                        node_id: node_id!(parser, parser.span(start)),
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
