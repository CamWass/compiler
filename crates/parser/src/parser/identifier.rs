//! 12.1 Identifiers
use super::*;
use crate::token::Keyword;

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

        let w = match self.input.cur() {
            Word(..) => match self.input.bump() {
                Word(w) => w,
                _ => unreachable!(),
            },
            _ => syntax_error!(self, SyntaxError::ExpectedIdent),
        };

        Ok(self.new_ident(w.get_name_id(), self.span(start)))
    }

    /// Identifier
    ///
    /// In strict mode, "yield" is SyntaxError if matched.
    pub(super) fn parse_ident(&mut self, incl_yield: bool, incl_await: bool) -> PResult<Ident> {
        let start = self.input.cur_pos();

        let word = self.parse_with(|parser| {
            let w = match parser.input.cur() {
                Word(..) => match parser.input.bump() {
                    Word(w) => w,
                    _ => unreachable!(),
                },
                _ => syntax_error!(parser, SyntaxError::ExpectedIdent),
            };

            // Spec:
            // It is a Syntax Error if this phrase is contained in strict mode code and the
            // StringValue of IdentifierName is: "implements", "interface", "let",
            // "package", "private", "protected",  "public", "static", or "yield".
            match w {
                Word::Ident(id_for_built_in!("enum")) => {
                    parser.emit_err(parser.input.prev_span(), SyntaxError::InvalidIdentInStrict);
                }
                Word::Keyword(Keyword::Yield)
                | Word::Ident(id_for_built_in!("static"))
                | Word::Ident(id_for_built_in!("implements"))
                | Word::Ident(id_for_built_in!("interface"))
                | Word::Ident(id_for_built_in!("let"))
                | Word::Ident(id_for_built_in!("package"))
                | Word::Ident(id_for_built_in!("private"))
                | Word::Ident(id_for_built_in!("protected"))
                | Word::Ident(id_for_built_in!("public")) => {
                    parser.emit_strict_mode_err(
                        parser.input.prev_span(),
                        SyntaxError::InvalidIdentInStrict,
                    );
                }
                _ => {}
            }

            // TODO:
            // Spec:
            // It is a Syntax Error if StringValue of IdentifierName is the same String
            // value as the StringValue of any ReservedWord except for yield or await.

            match w {
                Word::Keyword(Keyword::Await) if parser.input.syntax().typescript() => {
                    Ok(id_for_built_in!("await"))
                }
                // It is a Syntax Error if the goal symbol of the syntactic grammar is Module
                // and the StringValue of IdentifierName is "await".
                Word::Keyword(Keyword::Await) if parser.ctx().is_module() => {
                    syntax_error!(parser, parser.input.prev_span(), SyntaxError::ExpectedIdent)
                }
                Word::Keyword(Keyword::This) if parser.input.syntax().typescript() => {
                    Ok(id_for_built_in!("this"))
                }
                Word::Keyword(Keyword::Let) => Ok(id_for_built_in!("let")),
                Word::Ident(ident) => Ok(ident),
                Word::Keyword(Keyword::Yield) if incl_yield => Ok(id_for_built_in!("yield")),
                Word::Keyword(Keyword::Await) if incl_await => Ok(id_for_built_in!("await")),
                Word::Keyword(..) | Word::Null | Word::True | Word::False => {
                    syntax_error!(parser, parser.input.prev_span(), SyntaxError::ExpectedIdent)
                }
            }
        })?;

        Ok(self.new_ident(word, self.span(start)))
    }
}

pub(super) enum PrivateNameOrIdentifier {
    PrivateName(PrivateName),
    Identifier(Ident),
}

pub(super) trait MaybeOptionalIdentParser<Ident> {
    fn parse_maybe_opt_binding_ident(&mut self) -> PResult<Ident>;
}
impl MaybeOptionalIdentParser<Ident> for Parser<'_> {
    fn parse_maybe_opt_binding_ident(&mut self) -> PResult<Ident> {
        self.parse_binding_ident().map(|i| i.id)
    }
}
impl MaybeOptionalIdentParser<Option<Ident>> for Parser<'_> {
    fn parse_maybe_opt_binding_ident(&mut self) -> PResult<Option<Ident>> {
        self.parse_opt_binding_ident().map(|opt| opt.map(|i| i.id))
    }
}
