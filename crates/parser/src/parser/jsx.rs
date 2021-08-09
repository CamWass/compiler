use super::*;
use either::Either;
use global_common::{Span, Spanned, SyntaxContext};

impl<'ast, I: Tokens> Parser<'ast, I> {
    /// Parse next token as JSX identifier
    pub(super) fn parse_jsx_ident(&mut self) -> PResult<&'ast Ident> {
        debug_assert!(self.input.syntax().jsx());

        let ctx = self.ctx();
        match *cur!(self, true)? {
            Token::JSXName { .. } => match self.input.bump() {
                Token::JSXName { name } => {
                    let span = self.input.prev_span();
                    let ident = alloc!(
                        self,
                        Ident {
                            sym: name,
                            span,
                            optional: false
                        }
                    );
                    Ok(ident)
                }
                _ => unreachable!(),
            },
            _ if ctx.in_forced_jsx_context => self.parse_ident_ref(),
            _ => unexpected!(self, "jsx identifier"),
        }
    }

    /// Parse namespaced identifier.
    pub(super) fn parse_jsx_namespaced_name(&mut self) -> PResult<JSXAttrName<'ast>> {
        debug_assert!(self.input.syntax().jsx());

        let ns = self.parse_jsx_ident()?;
        if !eat!(self, ':') {
            return Ok(JSXAttrName::Ident(ns));
        }

        let name = self.parse_jsx_ident()?;
        let namespaced_name = alloc!(self, JSXNamespacedName { ns, name });
        Ok(JSXAttrName::JSXNamespacedName(namespaced_name))
    }

    /// Parses element name in any form - namespaced, member or single
    /// identifier.
    pub(super) fn parse_jsx_element_name(&mut self) -> PResult<JSXElementName<'ast>> {
        debug_assert!(self.input.syntax().jsx());

        let mut node = match self.parse_jsx_namespaced_name()? {
            JSXAttrName::Ident(i) => JSXElementName::Ident(i),
            JSXAttrName::JSXNamespacedName(i) => JSXElementName::JSXNamespacedName(i),
        };
        while eat!(self, '.') {
            let prop = self.parse_jsx_ident()?;
            let expr = alloc!(
                self,
                JSXMemberExpr {
                    obj: match node {
                        JSXElementName::Ident(i) => JSXObject::Ident(i),
                        JSXElementName::JSXMemberExpr(i) => JSXObject::JSXMemberExpr(i),
                        _ => unimplemented!("JSXNamespacedName -> JSXObject"),
                    },
                    prop,
                }
            );
            let new_node = JSXElementName::JSXMemberExpr(expr);
            node = new_node;
        }
        Ok(node)
    }

    /// Parses any type of JSX attribute value.
    ///
    /// TODO(kdy1): Change return type to JSXAttrValue
    pub(super) fn parse_jsx_attr_value(&mut self) -> PResult<JSXAttrValue<'ast>> {
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
                    Either::Right(n) => Ok(JSXAttrValue::JSXElement(n)),
                }
            }

            _ => syntax_error!(self, span!(self, start), SyntaxError::InvalidJSXValue),
        }
    }

    /// JSXEmptyExpression is unique type since it doesn't actually parse
    /// anything, and so it should start at the end of last read token (left
    /// brace) and finish at the beginning of the next one (right brace).
    pub(super) fn parse_jsx_empty_expr(&mut self) -> PResult<&'ast JSXEmptyExpr> {
        debug_assert!(self.input.syntax().jsx());
        let start = self.input.cur_pos();

        Ok(alloc!(
            self,
            JSXEmptyExpr {
                span: Span::new(start, start, SyntaxContext::empty()),
            }
        ))
    }

    /// Parse JSX spread child
    pub(super) fn parse_jsx_spread_child(&mut self) -> PResult<&'ast JSXSpreadChild<'ast>> {
        debug_assert!(self.input.syntax().jsx());
        let start = self.input.cur_pos();
        expect!(self, '{');
        expect!(self, "...");
        let expr = self.parse_expr()?;
        expect!(self, '}');

        Ok(alloc!(
            self,
            JSXSpreadChild {
                span: span!(self, start),
                expr,
            }
        ))
    }

    /// Parses JSX expression enclosed into curly brackets.
    pub(super) fn parse_jsx_expr_container(&mut self) -> PResult<&'ast JSXExprContainer<'ast>> {
        debug_assert!(self.input.syntax().jsx());

        let start = self.input.cur_pos();
        self.input.bump();
        let expr = if is!(self, '}') {
            self.parse_jsx_empty_expr().map(JSXExpr::JSXEmptyExpr)?
        } else {
            self.parse_expr().map(JSXExpr::Expr)?
        };
        expect!(self, '}');
        Ok(alloc!(
            self,
            JSXExprContainer {
                span: span!(self, start),
                expr,
            }
        ))
    }

    /// Parses following JSX attribute name-value pair.
    pub(super) fn parse_jsx_attr(&mut self) -> PResult<JSXAttrOrSpread<'ast>> {
        debug_assert!(self.input.syntax().jsx());
        let start = self.input.cur_pos();

        if eat!(self, '{') {
            let dot3_start = self.input.cur_pos();
            expect!(self, "...");
            let dot3_token = span!(self, dot3_start);
            let expr = self.parse_assignment_expr()?;
            expect!(self, '}');
            let spread = alloc!(self, SpreadElement { dot3_token, expr });
            return Ok(JSXAttrOrSpread::SpreadElement(spread));
        }

        let name = self.parse_jsx_namespaced_name()?;
        let value = if eat!(self, '=') {
            self.parse_jsx_attr_value().map(Some)?
        } else {
            None
        };

        let attr = alloc!(
            self,
            JSXAttr {
                span: span!(self, start),
                name,
                value,
            }
        );

        Ok(JSXAttrOrSpread::JSXAttr(attr))
    }

    /// Parses JSX opening tag starting after "<".
    pub(super) fn parse_jsx_opening_element_at(
        &mut self,
        start: BytePos,
    ) -> PResult<Either<&'ast JSXOpeningFragment, &'ast JSXOpeningElement<'ast>>> {
        debug_assert!(self.input.syntax().jsx());

        if eat!(self, JSXTagEnd) {
            let fragment = alloc!(
                self,
                JSXOpeningFragment {
                    span: span!(self, start),
                }
            );
            return Ok(Either::Left(fragment));
        }

        let name = self.parse_jsx_element_name()?;
        self.parse_jsx_opening_element_after_name(start, name)
            .map(Either::Right)
    }

    /// `jsxParseOpeningElementAfterName`
    pub(super) fn parse_jsx_opening_element_after_name(
        &mut self,
        start: BytePos,
        name: JSXElementName<'ast>,
    ) -> PResult<&'ast JSXOpeningElement<'ast>> {
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
        Ok(alloc!(
            self,
            JSXOpeningElement {
                span: span!(self, start),
                name,
                attrs,
                self_closing,
                type_args,
            }
        ))
    }

    /// Parses JSX closing tag starting after "</".
    fn parse_jsx_closing_element_at(
        &mut self,
        start: BytePos,
    ) -> PResult<Either<&'ast JSXClosingFragment, &'ast JSXClosingElement<'ast>>> {
        debug_assert!(self.input.syntax().jsx());

        if eat!(self, JSXTagEnd) {
            let fragment = alloc!(
                self,
                JSXClosingFragment {
                    span: span!(self, start),
                }
            );
            return Ok(Either::Left(fragment));
        }

        let name = self.parse_jsx_element_name()?;
        expect!(self, JSXTagEnd);
        let elem = alloc!(
            self,
            JSXClosingElement {
                span: span!(self, start),
                name,
            }
        );
        Ok(Either::Right(elem))
    }

    /// Parses entire JSX element, including it"s opening tag
    /// (starting after "<"), attributes, contents and closing tag.
    ///
    /// babel: `jsxParseElementAt`
    pub(super) fn parse_jsx_element_at(
        &mut self,
        start_pos: BytePos,
    ) -> PResult<Either<&'ast JSXFragment<'ast>, &'ast JSXElement<'ast>>> {
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
                                Either::Right(e) => JSXElementChild::from(e),
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
                (Either::Left(opening), Some(Either::Left(closing))) => {
                    let fragment = alloc!(
                        self,
                        JSXFragment {
                            span,
                            opening,
                            children,
                            closing,
                        }
                    );
                    Either::Left(fragment)
                }
                (Either::Right(opening), None) => {
                    let elem = alloc!(
                        self,
                        JSXElement {
                            span,
                            opening,
                            children,
                            closing: None,
                        }
                    );
                    Either::Right(elem)
                }
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
                    let elem = alloc!(
                        self,
                        JSXElement {
                            span,
                            opening,
                            children,
                            closing: Some(closing),
                        }
                    );
                    Either::Right(elem)
                }
                _ => unreachable!(),
            })
        })
    }

    /// Parses entire JSX element from current position.
    ///
    /// babel: `jsxParseElement`
    pub(super) fn parse_jsx_element(
        &mut self,
    ) -> PResult<Either<&'ast JSXFragment<'ast>, &'ast JSXElement<'ast>>> {
        debug_assert!(self.input.syntax().jsx());
        debug_assert!({
            match *cur!(self, true)? {
                Token::JSXTagStart | tok!('<') => true,
                _ => false,
            }
        });

        let start_pos = self.input.cur_pos();

        self.parse_jsx_element_at(start_pos)
    }

    pub(super) fn parse_jsx_text(&mut self) -> PResult<&'ast JSXText> {
        debug_assert!(self.input.syntax().jsx());
        debug_assert!({
            match cur!(self, false) {
                Ok(&Token::JSXText { .. }) => true,
                _ => false,
            }
        });
        let token = self.input.bump();
        let span = self.input.prev_span();
        match token {
            Token::JSXText { raw } => Ok(JSXText {
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

impl<'ast> IsFragment for Either<&'ast JSXOpeningFragment, &'ast JSXOpeningElement<'ast>> {
    fn is_fragment(&self) -> bool {
        match *self {
            Either::Left(..) => true,
            _ => false,
        }
    }
}

impl<'ast> IsFragment for Either<&'ast JSXClosingFragment, &'ast JSXClosingElement<'ast>> {
    fn is_fragment(&self) -> bool {
        match *self {
            Either::Left(..) => true,
            _ => false,
        }
    }
}
impl<T: IsFragment> IsFragment for Option<T> {
    fn is_fragment(&self) -> bool {
        self.as_ref().map(|s| s.is_fragment()).unwrap_or(false)
    }
}

fn get_qualified_jsx_name<'ast>(name: JSXElementName<'ast>) -> JsWord {
    fn get_qualified_obj_name<'ast>(obj: JSXObject<'ast>) -> JsWord {
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
