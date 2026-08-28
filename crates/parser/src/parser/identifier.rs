//! 12.1 Identifiers
use super::*;

impl Parser<'_> {
    pub(super) fn new_ident(&mut self, name: NameId, span: Span) -> Ident {
        Ident {
            node_id: node_id!(self, span),
            name,
        }
    }

    pub(super) fn parse_maybe_private_name(&mut self) -> PResult<PrivateNameOrIdentifier> {
        let is_private = self.is(tok!('#'));

        if is_private {
            self.parse_private_name()
                .map(PrivateNameOrIdentifier::PrivateName)
        } else {
            self.parse_ident_name()
                .map(PrivateNameOrIdentifier::Identifier)
        }
    }

    pub(super) fn parse_private_name(&mut self) -> PResult<PrivateName> {
        let start = self.input.cur_pos();
        self.assert_and_bump(tok!('#'));

        let hash_end = self.input.prev_span().hi;
        if self.input.cur_pos() - hash_end != BytePos(0) {
            syntax_error!(
                self,
                self.span(start),
                SyntaxError::SpaceBetweenHashAndIdent
            );
        }

        let id = self.parse_ident_name()?;
        Ok(PrivateName {
            node_id: node_id!(self, self.span(start)),
            id,
        })
    }

    /// LabelIdentifier
    pub(super) fn parse_label_ident(&mut self) -> PResult<Ident> {
        let ctx = self.ctx();

        self.parse_ident(!ctx.in_generator(), !ctx.in_async())
    }

    /// Use this when spec says "IdentifierName".
    /// This allows idents like `catch`.
    pub(super) fn parse_ident_name(&mut self) -> PResult<Ident> {
        let start = self.input.cur_pos();

        let w = if self.input.cur().is_word() {
            self.input.expect_word_token_and_bump()
        } else {
            syntax_error!(self, SyntaxError::ExpectedIdent)
        };

        Ok(self.new_ident(w, self.span(start)))
    }

    /// Identifier
    ///
    /// In strict mode, "yield" is SyntaxError if matched.
    pub(super) fn parse_ident(&mut self, incl_yield: bool, incl_await: bool) -> PResult<Ident> {
        let start = self.input.cur_pos();

        let is_keyword = self.input.cur().is_keyword();

        let word = if self.input.cur().is_word() {
            self.input.expect_word_token_and_bump()
        } else {
            syntax_error!(self, SyntaxError::ExpectedIdent)
        };

        // Spec:
        // It is a Syntax Error if this phrase is contained in strict mode code and the
        // StringValue of IdentifierName is: "implements", "interface", "let",
        // "package", "private", "protected",  "public", "static", or "yield".
        match word {
            id_for_built_in!("enum") => {
                self.emit_err(self.input.prev_span(), SyntaxError::InvalidIdentInStrict);
            }
            id_for_built_in!("yield")
            | id_for_built_in!("static")
            | id_for_built_in!("implements")
            | id_for_built_in!("interface")
            | id_for_built_in!("let")
            | id_for_built_in!("package")
            | id_for_built_in!("private")
            | id_for_built_in!("protected")
            | id_for_built_in!("public") => {
                self.emit_strict_mode_err(
                    self.input.prev_span(),
                    SyntaxError::InvalidIdentInStrict,
                );
            }
            _ => {}
        }

        // TODO:
        // Spec:
        // It is a Syntax Error if StringValue of IdentifierName is the same String
        // value as the StringValue of any ReservedWord except for yield or await.

        match word {
            id_for_built_in!("await") if self.input.syntax().typescript() => {}
            // It is a Syntax Error if the goal symbol of the syntactic grammar is Module
            // and the StringValue of IdentifierName is "await".
            id_for_built_in!("await") if self.ctx().is_module() => {
                syntax_error!(self, self.input.prev_span(), SyntaxError::ExpectedIdent)
            }
            id_for_built_in!("this") if self.input.syntax().typescript() => {}
            id_for_built_in!("let") => {}
            id_for_built_in!("yield") if incl_yield => {}
            id_for_built_in!("await") if incl_await => {}
            id_for_built_in!("null") | id_for_built_in!("true") | id_for_built_in!("false") => {
                syntax_error!(self, self.input.prev_span(), SyntaxError::ExpectedIdent)
            }
            _ if is_keyword => {
                syntax_error!(self, self.input.prev_span(), SyntaxError::ExpectedIdent)
            }
            _ => {}
        }

        Ok(self.new_ident(word, self.span(start)))
    }
}

pub(super) enum PrivateNameOrIdentifier {
    PrivateName(PrivateName),
    Identifier(Ident),
}
