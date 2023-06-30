use super::*;
use either::Either;
use global_common::{Span, Spanned, SyntaxContext};

impl<I: Tokens> Parser<I> {
    /// Parse next token as JSX identifier
    pub(super) fn parse_jsx_ident(&mut self) -> PResult<Ident> {
        debug_assert!(self.input.syntax().jsx());

        let ctx = self.ctx();
        match *cur!(self, true)? {
            Token::JSXName { .. } => match self.input.bump() {
                Token::JSXName { name } => {
                    let span = self.input.prev_span();
                    Ok(self.new_ident(name, span))
                }
                _ => unreachable!(),
            },
            _ if ctx.in_forced_jsx_context => self.parse_ident_ref(),
            _ => unexpected!(self, "jsx identifier"),
        }
    }

    /// Parse namespaced identifier.
    pub(super) fn parse_jsx_namespaced_name(&mut self) -> PResult<JSXAttrName> {
        debug_assert!(self.input.syntax().jsx());

        let ns = self.parse_jsx_ident()?;
        if !eat!(self, ':') {
            return Ok(JSXAttrName::Ident(ns));
        }

        let name = self.parse_jsx_ident()?;
        Ok(JSXAttrName::JSXNamespacedName(JSXNamespacedName {
            node_id: node_id!(self),
            ns,
            name,
        }))
    }

    /// Parses element name in any form - namespaced, member or single
    /// identifier.
    pub(super) fn parse_jsx_element_name(&mut self) -> PResult<JSXElementName> {
        debug_assert!(self.input.syntax().jsx());

        let mut node = match self.parse_jsx_namespaced_name()? {
            JSXAttrName::Ident(i) => JSXElementName::Ident(i),
            JSXAttrName::JSXNamespacedName(i) => JSXElementName::JSXNamespacedName(i),
        };
        while eat!(self, '.') {
            let prop = self.parse_jsx_ident()?;
            let new_node = JSXElementName::JSXMemberExpr(JSXMemberExpr {
                node_id: node_id!(self),
                obj: match node {
                    JSXElementName::Ident(i) => JSXObject::Ident(i),
                    JSXElementName::JSXMemberExpr(i) => JSXObject::JSXMemberExpr(Box::new(i)),
                    _ => unimplemented!("JSXNamespacedName -> JSXObject"),
                },
                prop,
            });
            node = new_node;
        }
        Ok(node)
    }

    /// Parses any type of JSX attribute value.
    ///
    /// TODO(kdy1): Change return type to JSXAttrValue
    pub(super) fn parse_jsx_attr_value(&mut self) -> PResult<JSXAttrValue> {
        debug_assert!(self.input.syntax().jsx());

        let start = self.input.cur_pos();

        match *cur!(self, true)? {
            tok!('{') => {
                let node = self.parse_jsx_expr_container()?;

                match node.expr {
                    JSXExpr::JSXEmptyExpr(..) => {
                        syntax_error!(self, span!(self, start), SyntaxError::EmptyJSXAttr)
                    }
                    JSXExpr::Expr(..) => Ok(node.into()),
                }
            }
            Token::Str { .. } => {
                let lit = self.parse_lit()?;
                Ok(JSXAttrValue::Lit(lit))
            }
            Token::JSXTagStart => {
                let expr = self.parse_jsx_element()?;
                match expr {
                    Either::Left(n) => Ok(JSXAttrValue::JSXFragment(n)),
                    Either::Right(n) => Ok(JSXAttrValue::JSXElement(Box::new(n))),
                }
            }

            _ => syntax_error!(self, span!(self, start), SyntaxError::InvalidJSXValue),
        }
    }

    /// JSXEmptyExpression is unique type since it doesn't actually parse
    /// anything, and so it should start at the end of last read token (left
    /// brace) and finish at the beginning of the next one (right brace).
    pub(super) fn parse_jsx_empty_expr(&mut self) -> PResult<JSXEmptyExpr> {
        debug_assert!(self.input.syntax().jsx());
        let start = self.input.cur_pos();

        Ok(JSXEmptyExpr {
            node_id: node_id!(self),
            span: Span::new(start, start, SyntaxContext::empty()),
        })
    }

    /// Parse JSX spread child
    pub(super) fn parse_jsx_spread_child(&mut self) -> PResult<JSXSpreadChild> {
        debug_assert!(self.input.syntax().jsx());
        let start = self.input.cur_pos();
        expect!(self, '{');
        expect!(self, "...");
        let expr = self.parse_expr()?;
        expect!(self, '}');

        Ok(JSXSpreadChild {
            node_id: node_id!(self),
            span: span!(self, start),
            expr,
        })
    }

    /// Parses JSX expression enclosed into curly brackets.
    pub(super) fn parse_jsx_expr_container(&mut self) -> PResult<JSXExprContainer> {
        debug_assert!(self.input.syntax().jsx());

        let start = self.input.cur_pos();
        self.input.bump();
        let expr = if is!(self, '}') {
            self.parse_jsx_empty_expr().map(JSXExpr::JSXEmptyExpr)?
        } else {
            self.parse_expr().map(JSXExpr::Expr)?
        };
        expect!(self, '}');
        Ok(JSXExprContainer {
            node_id: node_id!(self),
            span: span!(self, start),
            expr,
        })
    }

    /// Parses following JSX attribute name-value pair.
    pub(super) fn parse_jsx_attr(&mut self) -> PResult<JSXAttrOrSpread> {
        debug_assert!(self.input.syntax().jsx());
        let start = self.input.cur_pos();

        if eat!(self, '{') {
            let dot3_start = self.input.cur_pos();
            expect!(self, "...");
            let dot3_token = span!(self, dot3_start);
            let expr = self.parse_assignment_expr()?;
            expect!(self, '}');
            return Ok(SpreadElement {
                node_id: node_id!(self),
                dot3_token,
                expr,
            }
            .into());
        }

        let name = self.parse_jsx_namespaced_name()?;
        let value = if eat!(self, '=') {
            self.parse_jsx_attr_value().map(Some)?
        } else {
            None
        };

        Ok(JSXAttr {
            node_id: node_id!(self),
            span: span!(self, start),
            name,
            value,
        }
        .into())
    }

    /// Parses JSX opening tag starting after "<".
    pub(super) fn parse_jsx_opening_element_at(
        &mut self,
        start: BytePos,
    ) -> PResult<Either<JSXOpeningFragment, JSXOpeningElement>> {
        debug_assert!(self.input.syntax().jsx());

        if eat!(self, JSXTagEnd) {
            return Ok(Either::Left(JSXOpeningFragment {
                node_id: node_id!(self),
                span: span!(self, start),
            }));
        }

        let name = self.parse_jsx_element_name()?;
        self.parse_jsx_opening_element_after_name(start, name)
            .map(Either::Right)
    }

    /// `jsxParseOpeningElementAfterName`
    pub(super) fn parse_jsx_opening_element_after_name(
        &mut self,
        start: BytePos,
        name: JSXElementName,
    ) -> PResult<JSXOpeningElement> {
        debug_assert!(self.input.syntax().jsx());

        let type_args = if self.input.syntax().typescript() && is!(self, '<') {
            self.try_parse_ts(|p| p.parse_ts_type_args().map(Some))
        } else {
            None
        };

        let mut attrs = vec![];
        while let Ok(..) = cur!(self, false) {
            if is!(self, '/') || is!(self, JSXTagEnd) {
                break;
            }

            let attr = self.parse_jsx_attr()?;
            attrs.push(attr);
        }
        let self_closing = eat!(self, '/');
        if !eat!(self, JSXTagEnd) & !(self.ctx().in_forced_jsx_context && eat!(self, '>')) {
            unexpected!(self, "> (jsx closing tag)");
        }
        Ok(JSXOpeningElement {
            node_id: node_id!(self),
            span: span!(self, start),
            name,
            attrs,
            self_closing,
        })
    }

    /// Parses JSX closing tag starting after "</".
    fn parse_jsx_closing_element_at(
        &mut self,
        start: BytePos,
    ) -> PResult<Either<JSXClosingFragment, JSXClosingElement>> {
        debug_assert!(self.input.syntax().jsx());

        if eat!(self, JSXTagEnd) {
            return Ok(Either::Left(JSXClosingFragment {
                node_id: node_id!(self),
                span: span!(self, start),
            }));
        }

        let name = self.parse_jsx_element_name()?;
        expect!(self, JSXTagEnd);
        Ok(Either::Right(JSXClosingElement {
            node_id: node_id!(self),
            span: span!(self, start),
            name,
        }))
    }

    /// Parses entire JSX element, including it"s opening tag
    /// (starting after "<"), attributes, contents and closing tag.
    ///
    /// babel: `jsxParseElementAt`
    pub(super) fn parse_jsx_element_at(
        &mut self,
        start_pos: BytePos,
    ) -> PResult<Either<JSXFragment, JSXElement>> {
        debug_assert!(self.input.syntax().jsx());

        let _ = cur!(self, true);
        let start = self.input.cur_pos();
        let forced_jsx_context = match self.input.bump() {
            tok!('<') => true,
            Token::JSXTagStart => false,
            _ => unreachable!(),
        };

        let ctx = Context {
            in_forced_jsx_context: forced_jsx_context,
            ..self.ctx()
        };
        self.with_ctx(ctx).parse_with(|p| {
            let opening_element = p.parse_jsx_opening_element_at(start_pos)?;
            let mut children = vec![];
            let mut closing_element = None;

            let self_closing = match opening_element {
                Either::Right(ref el) => el.self_closing,
                _ => false,
            };

            if !self_closing {
                'contents: loop {
                    match *cur!(p, true)? {
                        Token::JSXTagStart => {
                            let start = p.input.cur_pos();

                            if peeked_is!(p, '/') {
                                p.input.bump(); // JSXTagStart
                                let _ = cur!(p, true);
                                p.assert_and_bump(&tok!('/'));

                                closing_element =
                                    p.parse_jsx_closing_element_at(start).map(Some)?;
                                break 'contents;
                            }

                            children.push(p.parse_jsx_element_at(start).map(|e| match e {
                                Either::Left(e) => JSXElementChild::from(e),
                                Either::Right(e) => JSXElementChild::from(Box::new(e)),
                            })?);
                        }
                        Token::JSXText { .. } => {
                            children.push(p.parse_jsx_text().map(JSXElementChild::from)?)
                        }
                        tok!('{') => {
                            if peeked_is!(p, "...") {
                                children
                                    .push(p.parse_jsx_spread_child().map(JSXElementChild::from)?);
                            } else {
                                children
                                    .push(p.parse_jsx_expr_container().map(JSXElementChild::from)?);
                            }
                        }
                        _ => unexpected!(p, "< (jsx tag start), jsx text or {"),
                    }
                }
            }
            let span = span!(p, start);

            Ok(match (opening_element, closing_element) {
                (Either::Left(..), Some(Either::Right(closing))) => {
                    syntax_error!(p, closing.span(), SyntaxError::JSXExpectedClosingTagForLtGt);
                }
                (Either::Right(opening), Some(Either::Left(closing))) => {
                    syntax_error!(
                        p,
                        closing.span(),
                        SyntaxError::JSXExpectedClosingTag {
                            tag: get_qualified_jsx_name(&opening.name)
                        }
                    );
                }
                (Either::Left(opening), Some(Either::Left(closing))) => Either::Left(JSXFragment {
                    node_id: node_id!(p),
                    span,
                    opening,
                    children,
                    closing,
                }),
                (Either::Right(opening), None) => Either::Right(JSXElement {
                    node_id: node_id!(p),
                    span,
                    opening,
                    children,
                    closing: None,
                }),
                (Either::Right(opening), Some(Either::Right(closing))) => {
                    if get_qualified_jsx_name(&closing.name)
                        != get_qualified_jsx_name(&opening.name)
                    {
                        syntax_error!(
                            p,
                            closing.span(),
                            SyntaxError::JSXExpectedClosingTag {
                                tag: get_qualified_jsx_name(&opening.name)
                            }
                        );
                    }
                    Either::Right(JSXElement {
                        node_id: node_id!(p),
                        span,
                        opening,
                        children,
                        closing: Some(closing),
                    })
                }
                _ => unreachable!(),
            })
        })
    }

    /// Parses entire JSX element from current position.
    ///
    /// babel: `jsxParseElement`
    pub(super) fn parse_jsx_element(&mut self) -> PResult<Either<JSXFragment, JSXElement>> {
        debug_assert!(self.input.syntax().jsx());
        debug_assert!({ matches!(*cur!(self, true)?, Token::JSXTagStart | tok!('<')) });

        let start_pos = self.input.cur_pos();

        self.parse_jsx_element_at(start_pos)
    }

    pub(super) fn parse_jsx_text(&mut self) -> PResult<JSXText> {
        debug_assert!(self.input.syntax().jsx());
        debug_assert!({ matches!(cur!(self, false), Ok(&Token::JSXText { .. })) });
        let token = self.input.bump();
        let span = self.input.prev_span();
        match token {
            Token::JSXText { raw } => Ok(JSXText {
                node_id: node_id!(self),
                span,
                // TODO
                value: raw.clone(),
                raw,
            }),
            _ => unreachable!(),
        }
    }
}

trait IsFragment {
    fn is_fragment(&self) -> bool;
}

impl IsFragment for Either<JSXOpeningFragment, JSXOpeningElement> {
    fn is_fragment(&self) -> bool {
        matches!(*self, Either::Left(..))
    }
}

impl IsFragment for Either<JSXClosingFragment, JSXClosingElement> {
    fn is_fragment(&self) -> bool {
        matches!(*self, Either::Left(..))
    }
}
impl<T: IsFragment> IsFragment for Option<T> {
    fn is_fragment(&self) -> bool {
        self.as_ref().map(|s| s.is_fragment()).unwrap_or(false)
    }
}

fn get_qualified_jsx_name(name: &JSXElementName) -> JsWord {
    fn get_qualified_obj_name(obj: &JSXObject) -> JsWord {
        match *obj {
            JSXObject::Ident(ref i) => i.sym.clone(),
            JSXObject::JSXMemberExpr(ref member) => format!(
                "{}.{}",
                get_qualified_obj_name(&member.obj),
                member.prop.sym
            )
            .into(),
        }
    }
    match *name {
        JSXElementName::Ident(ref i) => i.sym.clone(),
        JSXElementName::JSXNamespacedName(JSXNamespacedName {
            ref ns, ref name, ..
        }) => format!("{}:{}", ns.sym, name.sym).into(),
        JSXElementName::JSXMemberExpr(JSXMemberExpr {
            ref obj, ref prop, ..
        }) => format!("{}.{}", get_qualified_obj_name(obj), prop.sym).into(),
    }
}
