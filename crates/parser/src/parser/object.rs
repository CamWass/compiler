//! Parser for object literal.

use crate::{
    ast::*,
    parser::{class_and_fn::is_not_this, util::ParseObject, Context, Parser, Tokens},
    token::{Token, Word},
};
use global_common::{Span, Spanned};
use swc_atoms::js_word;

impl<'a, I: Tokens> Parser<I> {
    /// Parse a object literal or object pattern.
    pub(super) fn parse_object<T>(&mut self) -> T
    where
        Self: ParseObject<T>,
    {
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

            let prop = self.parse_object_prop();
            props.push(prop);
        }

        self.make_object(span!(self, start), props)
    }

    /// spec: 'PropertyName'
    pub(super) fn parse_prop_name(&mut self) -> PropName {
        let ctx = self.ctx();
        self.with_ctx(Context {
            in_property_name: true,
            ..ctx
        })
        .parse_with(|p| {
            let start = p.input.cur_pos();

            let v = match *cur!(p, true) {
                Token::Str { .. } => match p.input.bump() {
                    Token::Str { value, has_escape } => PropName::Str(Str {
                        span: span!(p, start),
                        value,
                        has_escape,
                        kind: StrKind::Normal {
                            contains_quote: true,
                        },
                    }),
                    _ => unreachable!(),
                },
                Token::Num(_) => match p.input.bump() {
                    Token::Num(value) => PropName::Num(Number {
                        span: span!(p, start),
                        value,
                    }),
                    _ => unreachable!(),
                },
                Token::BigInt(_) => match p.input.bump() {
                    Token::BigInt(value) => PropName::BigInt(BigInt {
                        span: span!(p, start),
                        value,
                    }),
                    _ => unreachable!(),
                },
                Word(..) => match p.input.bump() {
                    Word(w) => PropName::Ident(Ident::new(w.into(), span!(p, start))),
                    _ => unreachable!(),
                },
                tok!('[') => {
                    p.input.bump();
                    // let inner_start = p.input.cur_pos();

                    let expr = p.include_in_expr(true).parse_assignment_expr();

                    // if p.syntax().typescript() && p.input.is(&tok!(',')) {
                    //     let mut exprs = vec![expr];

                    //     while p.input.eat(&tok!(',')) {
                    //         exprs.push(p.include_in_expr(true).parseMaybeAssign());
                    //     }

                    //     p.emit_err(span!(p, inner_start), SyntaxError::TS1171);

                    //     expr = Box::new(
                    //         SeqExpr {
                    //             span: span!(p, inner_start),
                    //             exprs,
                    //         }
                    //         .into(),
                    //     );
                    // }

                    expect!(p, ']');

                    PropName::Computed(ComputedPropName {
                        span: span!(p, start),
                        expr,
                    })
                }
                _ => unexpected!(
                    p,
                    "identifier, string literal, numeric literal or [ for the computed key"
                ),
            };

            v
        })
    }
}

impl<I: Tokens> ParseObject<Box<Expr>> for Parser<I> {
    type Prop = PropOrSpread;

    fn make_object(&mut self, span: Span, props: Vec<Self::Prop>) -> Box<Expr> {
        Box::new(Expr::Object(ObjectLit { span, props }))
    }

    /// spec: 'PropertyDefinition'
    fn parse_object_prop(&mut self) -> Self::Prop {
        let start = self.input.cur_pos();
        // Parse as 'MethodDefinition'

        if self.input.eat(&tok!("...")) {
            // spread element
            let dot3_token = span!(self, start);

            let expr = self.include_in_expr(true).parse_assignment_expr();

            return PropOrSpread::Spread(SpreadElement { dot3_token, expr });
        }

        if self.input.eat(&tok!('*')) {

            let name = self.parse_prop_name();
            let function = self.parse_fn_args_body(
                // no decorator in an object literal
                vec![],
                start,
                |p| p.parse_unique_formal_params(),
                false,
                true,
            );
            return PropOrSpread::Prop(Box::new(Prop::Method(MethodProp {
                key: name,
                function,
            })));
        }

        // let has_modifiers = self.eat_any_ts_modifier();
        let has_modifiers = false;
        let modifiers_span = self.input.prev_span();

        let key = self.parse_prop_name();

        // if self.input.syntax().typescript()
        //     && !is_one_of!(self, '(', '[', ':', ',', '?', '=', '*', IdentName)
        //     && !(self.input.syntax().typescript() && is!(self, '<'))
        //     && !(is!(self, '}')
        //         && match key {
        //             PropName::Ident(..) => true,
        //             _ => false,
        //         })
        // {
        //     panic!("TS1005 at {:?}", self.input.cur_span());
        //     // self.emit_err(self.input.cur_span(), SyntaxError::TS1005);
        //     // return PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
        //     //     key,
        //     //     value: Box::new(Expr::Invalid(Invalid {
        //     //         span: span!(self, start),
        //     //     })),
        //     // })));
        // }

        //
        // {[computed()]: a,}
        // { 'a': a, }
        // { 0: 1, }
        // { a: expr, }
        if self.input.eat(&tok!(':')) {
            let value = self.include_in_expr(true).parse_assignment_expr();
            return PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp { key, value })));
        }

        // Handle `a(){}` (and async(){} / get(){} / set(){})
        if
        /*(self.input.syntax().typescript() && is!(self, '<')) ||*/
        is!(self, '(') {
            let function = self.parse_fn_args_body(
                // no decorator in an object literal
                vec![],
                start,
                |p| p.parse_unique_formal_params(),
                false,
                false,
            );
            return PropOrSpread::Prop(Box::new(Prop::Method(MethodProp { key, function })));
        }

        let ident = match key {
            PropName::Ident(ident) => ident,
            // TODO
            _ => unexpected!(self, "identifier"),
        };

        if self.input.eat(&tok!('?')) {
            panic!("TS1162 at {:?}", self.input.prev_span());
            // self.emit_err(self.input.prev_span(), SyntaxError::TS1162);
        }

        // `ident` from parse_prop_name is parsed as 'IdentifierName'
        // It means we should check for invalid expressions like { for, }
        if is_one_of!(self, '=', ',', '}') {
            let is_reserved_word = { self.ctx().is_reserved_word(&ident.sym) };
            if is_reserved_word {
                panic!("ReservedWordInObjShorthandOrPat at {:?}", ident.span);
                // self.emit_err(ident.span, SyntaxError::ReservedWordInObjShorthandOrPat);
            }

            if self.input.eat(&tok!('=')) {
                let value = self.include_in_expr(true).parse_assignment_expr();
                return PropOrSpread::Prop(Box::new(Prop::Assign(AssignProp {
                    key: ident,
                    value,
                })));
            }

            return PropOrSpread::Prop(Box::new(Prop::from(ident)));
        }

        // get a(){}
        // set a(v){}
        // async a(){}

        match ident.sym {
            js_word!("get") | js_word!("set") | js_word!("async") => {
                if has_modifiers {
                    // self.emit_err(modifiers_span, SyntaxError::TS1042);
                    panic!("TS1042 at {:?}", modifiers_span);
                }

                let is_generator = ident.sym == js_word!("async") && self.input.eat(&tok!('*'));
                let key = self.parse_prop_name();
                let key_span = key.span();

                match ident.sym {
                    js_word!("get") => {
                        let function = self.parse_fn_args_body(
                            // no decorator in an object literal
                            vec![],
                            start,
                            |p| {
                                let params = p.parse_formal_params();

                                if params.iter().filter(|p| is_not_this(p)).count() != 0 {
                                    // p.emit_err(key_span, SyntaxError::TS1094);
                                    panic!("TS1094 at {:?}", key_span);
                                }

                                params
                            },
                            false,
                            false,
                        );

                        if function.type_params.is_some() {
                            // self.emit_err(type_params.unwrap().span(), SyntaxError::TS1094);
                            panic!("TS1094 at {:?}", function.type_params.unwrap().span());
                        }

                        // if self.input.syntax().typescript()
                        //     && self.input.target() == JscTarget::Es3
                        // {
                        //     self.emit_err(key_span, SyntaxError::TS1056);
                        // }

                        PropOrSpread::Prop(Box::new(Prop::Getter(GetterProp {
                            span: span!(self, start),
                            key,
                            type_ann: function.return_type,
                            body: function.body,
                        })))
                    }
                    js_word!("set") => {
                        let function = self.parse_fn_args_body(
                            // no decorator in an object literal
                            vec![],
                            start,
                            |p| {
                                let params = p.parse_formal_params();

                                if params.iter().filter(|p| is_not_this(p)).count() != 1 {
                                    // p.emit_err(key_span, SyntaxError::TS1094);
                                    panic!("TS1094 at {:?}", key_span);
                                }

                                if !params.is_empty() {
                                    if let Pat::Rest(..) = params[0].pat {
                                        panic!("RestPatInSetter at {:?}", params[0].span());
                                        // p.emit_err(params[0].span(), SyntaxError::RestPatInSetter);
                                    }
                                }

                                // if p.input.syntax().typescript()
                                //     && p.input.target() == JscTarget::Es3
                                // {
                                //     p.emit_err(key_span, SyntaxError::TS1056);
                                // }

                                params
                            },
                            false,
                            false,
                        );

                        if function.type_params.is_some() {
                            // self.emit_err(type_params.unwrap().span(), SyntaxError::TS1094);
                            panic!("TS1094 at {:?}", function.type_params.unwrap().span());
                        }

                        // debug_assert_eq!(params.len(), 1);
                        PropOrSpread::Prop(Box::new(Prop::Setter(SetterProp {
                            span: span!(self, start),
                            key,
                            body: function.body,
                            param: function
                                .params
                                .into_iter()
                                .map(|p| p.pat)
                                .next()
                                .unwrap_or_else(|| Pat::Invalid(Invalid { span: key_span })),
                        })))
                    }
                    js_word!("async") => {
                        let function = self.parse_fn_args_body(
                            // no decorator in an object literal
                            vec![],
                            start,
                            |p| p.parse_unique_formal_params(),
                            true,
                            is_generator,
                        );

                        PropOrSpread::Prop(Box::new(Prop::Method(MethodProp { key, function })))
                    }
                    _ => unreachable!(),
                }
            }
            _ => {
                unexpected!(self, "... , *,  (, [, :, , ?, = or an identifier")
                // if self.input.syntax().typescript() {
                //     unexpected!(
                //         self,
                //         "... , *,  (, [, :, , ?, =, an identifier, public, protected, private, \
                //          readonly, <."
                //     )
                // } else {
                //     unexpected!(self, "... , *,  (, [, :, , ?, = or an identifier")
                // }
            }
        }
    }
}

impl<I: Tokens> ParseObject<Pat> for Parser<I> {
    type Prop = ObjectPatProp;

    fn make_object(&mut self, span: Span, props: Vec<Self::Prop>) -> Pat {
        let len = props.len();
        for (i, p) in props.iter().enumerate() {
            if i == len - 1 {
                if let ObjectPatProp::Rest(ref rest) = p {
                    match *rest.arg {
                        Pat::Ident(..) => {}
                        _ => {
                            // syntax_error!(self, p.span(), SyntaxError::DotsWithoutIdentifier)
                            panic!("DotsWithoutIdentifier at {:?}", p.span());
                        }
                    }
                }
                continue;
            }

            if let ObjectPatProp::Rest(..) = p {
                // if self.syntax().early_errors() {
                //     syntax_error!(self, p.span(), SyntaxError::NonLastRestParam)
                // }
                panic!("NonLastRestParam at {:?}", p.span());
            }
        }

        // let optional =
        //     (self.input.syntax().dts() || self.ctx().in_declare) && self.input.eat(&tok!('?'));
        let optional = false;

        Pat::Object(ObjectPat {
            span,
            props,
            optional,
            type_ann: None,
        })
    }

    /// Production 'BindingProperty'
    fn parse_object_prop(&mut self) -> Self::Prop {
        let start = self.input.cur_pos();

        if self.input.eat(&tok!("...")) {
            // spread element
            let dot3_token = span!(self, start);

            let arg = Box::new(self.parse_binding_pat_or_ident());

            return ObjectPatProp::Rest(RestPat {
                span: span!(self, start),
                dot3_token,
                arg,
                type_ann: None,
            });
        }

        let key = self.parse_prop_name();
        if self.input.eat(&tok!(':')) {
            let value = Box::new(self.parse_binding_element());

            return ObjectPatProp::KeyValue(KeyValuePatProp { key, value });
        }
        let key = match key {
            PropName::Ident(ident) => ident,
            _ => unexpected!(self, "an identifier"),
        };

        let value = if self.input.eat(&tok!('=')) {
            Some(self.include_in_expr(true).parse_assignment_expr())
        } else {
            if self.ctx().is_reserved_word(&key.sym) {
                // self.emit_err(key.span, SyntaxError::ReservedWordInObjShorthandOrPat);
                panic!("ReservedWordInObjShorthandOrPat at {:?}", key.span);
            }

            None
        };

        ObjectPatProp::Assign(AssignPatProp {
            span: span!(self, start),
            key,
            value,
        })
    }
}
