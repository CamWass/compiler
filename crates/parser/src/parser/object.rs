//! Parser for object literal.

use super::{class_and_fn::is_not_this, util::ParseObject, *};
use global_common::Spanned;
use swc_atoms::js_word;

impl<'ast, I: Tokens> Parser<'ast, I> {
    /// Parse a object literal or object pattern.
    pub(super) fn parse_object<T>(&mut self) -> PResult<T>
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

            let prop = self.parse_object_prop()?;
            props.push(prop);
        }

        self.make_object(span!(self, start), props)
    }

    /// spec: 'PropertyName'
    pub(super) fn parse_prop_name(&mut self) -> PResult<PropName> {
        self.with_ctx(Context {
            in_property_name: true,
            ..self.ctx()
        })
        .parse_with(|parser| {
            let start = parser.input.cur_pos();

            let v = match *cur!(parser, true)? {
                Token::Str { .. } => match parser.input.bump() {
                    Token::Str { value, has_escape } => {
                        let s = alloc!(
                            parser,
                            Str {
                                span: span!(parser, start),
                                value,
                                has_escape,
                                kind: StrKind::Normal {
                                    contains_quote: true,
                                },
                            }
                        );
                        PropName::Str(s)
                    }
                    _ => unreachable!(),
                },
                Token::Num(_) => match parser.input.bump() {
                    Token::Num(value) => {
                        let num = alloc!(
                            parser,
                            Number {
                                span: span!(parser, start),
                                value,
                            }
                        );
                        PropName::Num(num)
                    }
                    _ => unreachable!(),
                },
                Token::BigInt(_) => match parser.input.bump() {
                    Token::BigInt(value) => {
                        let bigint = alloc!(
                            parser,
                            BigInt {
                                span: span!(parser, start),
                                value,
                            }
                        );
                        PropName::BigInt(bigint)
                    }
                    _ => unreachable!(),
                },
                Word(..) => match parser.input.bump() {
                    Word(w) => {
                        let ident = alloc!(
                            self,
                            Ident {
                                sym: w.into(),
                                span: span!(parser, start),
                                optional: false
                            }
                        );
                        PropName::Ident(ident)
                    }
                    _ => unreachable!(),
                },
                tok!('[') => {
                    parser.input.bump();
                    let inner_start = parser.input.cur_pos();

                    let mut expr = parser.include_in_expr(true).parse_assignment_expr()?;

                    if parser.syntax().typescript() && is!(parser, ',') {
                        let mut exprs = vec![expr];

                        while eat!(parser, ',') {
                            exprs.push(parser.include_in_expr(true).parse_assignment_expr()?);
                        }

                        parser.emit_err(span!(parser, inner_start), SyntaxError::TS1171);

                        let seq = alloc!(
                            parser,
                            SeqExpr {
                                span: span!(parser, inner_start),
                                exprs,
                            }
                        );

                        expr = Expr::Seq(seq);
                    }

                    expect!(parser, ']');

                    let computed = alloc!(
                        self,
                        ComputedPropName {
                            span: span!(parser, start),
                            expr,
                        }
                    );

                    PropName::Computed(computed)
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

impl<'ast, I: Tokens> ParseObject<Expr<'ast>> for Parser<'ast, I> {
    type Prop = PropOrSpread<'ast>;

    fn make_object(&mut self, span: Span, props: Vec<Self::Prop>) -> PResult<Expr<'ast>> {
        let expr = alloc!(self, ObjectLit { span, props });
        Ok(Expr::Object(expr))
    }

    /// spec: 'PropertyDefinition'
    fn parse_object_prop(&mut self) -> PResult<Self::Prop> {
        trace_cur!(self, parse_object_prop);

        let start = self.input.cur_pos();
        // Parse as 'MethodDefinition'

        if self.input.eat(&tok!("...")) {
            // spread element
            let dot3_token = span!(self, start);

            let expr = self.include_in_expr(true).parse_assignment_expr()?;

            let spread = alloc!(self, SpreadElement { dot3_token, expr });

            return Ok(PropOrSpread::Spread(spread));
        }

        if self.input.eat(&tok!('*')) {
            let name = self.parse_prop_name()?;
            return self
                .parse_fn_args_body(
                    // no decorator in an object literal
                    vec![],
                    start,
                    |parser| parser.parse_unique_formal_params(),
                    false,
                    true,
                )
                .map(|function| {
                    let method = alloc!(
                        self,
                        MethodProp {
                            key: name,
                            function,
                        }
                    );
                    PropOrSpread::Prop(Prop::Method(method))
                });
        }

        let has_modifiers = self.eat_any_ts_modifier()?;
        let modifiers_span = self.input.prev_span();

        let key = self.parse_prop_name()?;

        if self.input.syntax().typescript()
            && !is_one_of!(self, '(', '[', ':', ',', '?', '=', '*', IdentName, Str, Num)
            && !(self.input.syntax().typescript() && is!(self, '<'))
            && !(is!(self, '}')
                && match key {
                    PropName::Ident(..) => true,
                    _ => false,
                })
        {
            trace_cur!(self, parse_object_prop_error);

            self.emit_err(self.input.cur_span(), SyntaxError::TS1005);

            let value = alloc!(
                self,
                Invalid {
                    span: span!(self, start),
                }
            );

            let prop = alloc!(
                self,
                KeyValueProp {
                    key,
                    value: Expr::Invalid(value),
                }
            );
            return Ok(PropOrSpread::Prop(Prop::KeyValue(prop)));
        }
        //
        // {[computed()]: a,}
        // { 'a': a, }
        // { 0: 1, }
        // { a: expr, }
        if self.input.eat(&tok!(':')) {
            let value = self.include_in_expr(true).parse_assignment_expr()?;
            let prop = alloc!(self, KeyValueProp { key, value });
            return Ok(PropOrSpread::Prop(Prop::KeyValue(prop)));
        }

        // Handle `a(){}` (and async(){} / get(){} / set(){})
        if (self.input.syntax().typescript() && is!(self, '<')) || is!(self, '(') {
            return self
                .parse_fn_args_body(
                    // no decorator in an object literal
                    vec![],
                    start,
                    |parser| parser.parse_unique_formal_params(),
                    false,
                    false,
                )
                .map(|function| {
                    let method = alloc!(self, MethodProp { key, function });
                    Prop::Method(method)
                })
                .map(PropOrSpread::Prop);
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
                self.emit_err(ident.span, SyntaxError::ReservedWordInObjShorthandOrPat);
            }

            if self.input.eat(&tok!('=')) {
                let value = self.include_in_expr(true).parse_assignment_expr()?;
                let assign = alloc!(self, AssignProp { key: ident, value });
                return Ok(PropOrSpread::Prop(Prop::Assign(assign)));
            }

            return Ok(PropOrSpread::Prop(Prop::from(ident)));
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
                let key_span = key.span();

                match ident.sym {
                    js_word!("get") => self
                        .parse_fn_args_body(
                            // no decorator in an object literal
                            vec![],
                            start,
                            |parser| {
                                let params = parser.parse_formal_params()?;

                                if params.iter().filter(|param| is_not_this(param)).count() != 0 {
                                    parser.emit_err(key_span, SyntaxError::TS1094);
                                }

                                Ok(params)
                            },
                            false,
                            false,
                        )
                        .map(
                            |Function {
                                 body,
                                 type_params,
                                 return_type,
                                 ..
                             }| {
                                if let Some(type_params) = type_params {
                                    self.emit_err(type_params.span(), SyntaxError::TS1094);
                                }

                                if self.input.syntax().typescript()
                                    && self.input.target() == JscTarget::Es3
                                {
                                    self.emit_err(key_span, SyntaxError::TS1056);
                                }

                                let getter = alloc!(
                                    self,
                                    GetterProp {
                                        span: span!(self, start),
                                        key,
                                        type_ann: *return_type,
                                        body: *body,
                                    }
                                );

                                PropOrSpread::Prop(Prop::Getter(getter))
                            },
                        ),
                    js_word!("set") => self
                        .parse_fn_args_body(
                            // no decorator in an object literal
                            vec![],
                            start,
                            |parser| {
                                let params = parser.parse_formal_params()?;

                                if params.iter().filter(|param| is_not_this(param)).count() != 1 {
                                    parser.emit_err(key_span, SyntaxError::TS1094);
                                }

                                if !params.is_empty() {
                                    if let Pat::Rest(..) = params[0].pat {
                                        parser.emit_err(
                                            params[0].span(),
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
                        .map(
                            |Function {
                                 params,
                                 body,
                                 type_params,
                                 ..
                             }| {
                                if let Some(type_params) = type_params {
                                    self.emit_err(type_params.span(), SyntaxError::TS1094);
                                }

                                let param = params
                                    .into_iter()
                                    .map(|param| param.pat)
                                    .next()
                                    .unwrap_or_else(|| {
                                        Pat::Invalid(alloc!(self, Invalid { span: key_span }))
                                    });

                                let setter = alloc!(
                                    self,
                                    SetterProp {
                                        span: span!(self, start),
                                        key,
                                        body: *body,
                                        param,
                                    }
                                );

                                // debug_assert_eq!(params.len(), 1);
                                PropOrSpread::Prop(Prop::Setter(setter))
                            },
                        ),
                    js_word!("async") => self
                        .parse_fn_args_body(
                            // no decorator in an object literal
                            vec![],
                            start,
                            |parser| parser.parse_unique_formal_params(),
                            true,
                            is_generator,
                        )
                        .map(|function| {
                            let method = alloc!(self, MethodProp { key, function });
                            PropOrSpread::Prop(Prop::Method(method))
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

impl<'ast, I: Tokens> ParseObject<Pat<'ast>> for Parser<'ast, I> {
    type Prop = ObjectPatProp<'ast>;

    fn make_object(&mut self, span: Span, props: Vec<Self::Prop>) -> PResult<Pat<'ast>> {
        let len = props.len();
        for (i, p) in props.iter().enumerate() {
            if i == len - 1 {
                if let ObjectPatProp::Rest(ref rest) = p {
                    match rest.arg {
                        Pat::Ident(..) => {}
                        _ => syntax_error!(self, p.span(), SyntaxError::DotsWithoutIdentifier),
                    }
                }
                continue;
            }

            if let ObjectPatProp::Rest(..) = p {
                if self.syntax().early_errors() {
                    syntax_error!(self, p.span(), SyntaxError::NonLastRestParam)
                }
            }
        }

        let optional =
            (self.input.syntax().dts() || self.ctx().in_declare) && self.input.eat(&tok!('?'));

        let pat = alloc!(
            self,
            ObjectPat {
                span,
                props,
                optional,
                type_ann: None,
            }
        );

        Ok(Pat::Object(pat))
    }

    /// Production 'BindingProperty'
    fn parse_object_prop(&mut self) -> PResult<Self::Prop> {
        let start = self.input.cur_pos();

        if self.input.eat(&tok!("...")) {
            // spread element
            let dot3_token = span!(self, start);

            let arg = self.parse_binding_pat_or_ident()?;

            let rest = alloc!(
                self,
                RestPat {
                    span: span!(self, start),
                    dot3_token,
                    arg,
                    type_ann: None,
                }
            );

            return Ok(ObjectPatProp::Rest(rest));
        }

        let key = self.parse_prop_name()?;
        if self.input.eat(&tok!(':')) {
            let value = self.parse_binding_element()?;

            let prop = alloc!(self, KeyValuePatProp { key, value });

            return Ok(ObjectPatProp::KeyValue(prop));
        }
        let key = match key {
            PropName::Ident(ident) => ident,
            _ => unexpected!(self, "an identifier"),
        };

        let value = if self.input.eat(&tok!('=')) {
            self.include_in_expr(true)
                .parse_assignment_expr()
                .map(Some)?
        } else {
            if self.ctx().is_reserved_word(&key.sym) {
                self.emit_err(key.span, SyntaxError::ReservedWordInObjShorthandOrPat);
            }

            None
        };

        let assign = alloc!(
            self,
            AssignPatProp {
                span: span!(self, start),
                key,
                value,
            }
        );

        Ok(ObjectPatProp::Assign(assign))
    }
}
