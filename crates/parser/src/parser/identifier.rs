use crate::{
    ast::*,
    parser::{Parser, Tokens},
    token::{Keyword, Word},
};
use either::Either;
use global_common::BytePos;
use swc_atoms::js_word;

impl<'a, I: Tokens> Parser<I> {
    pub(super) fn parse_maybe_private_name(&mut self) -> Either<PrivateName, Ident> {
        let is_private = is!(self, '#');

        if is_private {
            Either::Left(self.parse_private_name())
        } else {
            Either::Right(self.parse_ident_name())
        }
    }

    pub(super) fn parse_private_name(&mut self) -> PrivateName {
        let start = self.input.cur_pos();
        self.assert_and_bump(&tok!('#'));

        let hash_end = self.input.prev_span().hi;
        if self.input.cur_pos() - hash_end != BytePos(0) {
            // syntax_error!(
            //     self,
            //     span!(self, start),
            //     SyntaxError::SpaceBetweenHashAndIdent
            // );
            panic!("SpaceBetweenHashAndIdent at {:?}", span!(self, start));
        }

        let id = self.parse_ident_name();
        PrivateName {
            span: span!(self, start),
            id,
        }
    }

    /// LabelIdentifier
    pub(super) fn parse_label_ident(&mut self) -> Ident {
        let ctx = self.ctx();

        self.parse_ident(!ctx.in_generator, !ctx.in_async)
    }

    /// Use this when spec says "IdentifierName".
    /// This allows idents like `catch`.
    pub(super) fn parse_ident_name(&mut self) -> Ident {
        let start = self.input.cur_pos();

        let w = match cur!(self, true) {
            &Word(..) => match self.input.bump() {
                Word(w) => w,
                _ => unreachable!(),
            },
            _ => {
                panic!("ExpectedIdent at {:?}", self.input.cur_span());
                // syntax_error!(self, SyntaxError::ExpectedIdent)
            }
        };

        Ident::new(w.into(), span!(self, start))
    }

    /// Identifier
    ///
    /// In strict mode, "yield" is SyntaxError if matched.
    pub(super) fn parse_ident(&mut self, incl_yield: bool, incl_await: bool) -> Ident {
        // trace_cur!(self, parse_ident);

        let start = self.input.cur_pos();

        let word = self.parse_with(|p| {
            let w = match cur!(p, true) {
                &Word(..) => match p.input.bump() {
                    Word(w) => w,
                    _ => unreachable!(),
                },
                _ => {
                    panic!("ExpectedIdent at {:?}", p.input.cur_span());
                    // syntax_error!(p, SyntaxError::ExpectedIdent)
                }
            };

            // Spec:
            // It is a Syntax Error if this phrase is contained in strict mode code and the
            // StringValue of IdentifierName is: "implements", "interface", "let",
            // "package", "private", "protected",  "public", "static", or "yield".
            match w {
                Word::Ident(js_word!("enum")) => {
                    // p.emit_err(p.input.prev_span(), SyntaxError::InvalidIdentInStrict);
                    panic!("InvalidIdentInStrict at {:?}", p.input.prev_span());
                }
                Word::Keyword(Keyword::Yield)
                | Word::Ident(js_word!("static"))
                | Word::Ident(js_word!("implements"))
                | Word::Ident(js_word!("interface"))
                | Word::Ident(js_word!("let"))
                | Word::Ident(js_word!("package"))
                | Word::Ident(js_word!("private"))
                | Word::Ident(js_word!("protected"))
                | Word::Ident(js_word!("public")) => {
                    // p.emit_strict_mode_err(p.input.prev_span(), SyntaxError::InvalidIdentInStrict);
                    panic!("InvalidIdentInStrict at {:?}", p.input.prev_span());
                }
                _ => {}
            }

            //TODO
            // Spec:
            // It is a Syntax Error if StringValue of IdentifierName is the same String
            // value as the StringValue of any ReservedWord except for yield or await.

            match w {
                // Word::Keyword(Keyword::Await) if p.input.syntax().typescript() => {
                //     Ok(js_word!("await"))
                // }

                // It is a Syntax Error if the goal symbol of the syntactic grammar is Module
                // and the StringValue of IdentifierName is "await".
                Word::Keyword(Keyword::Await) if p.ctx().module => {
                    // syntax_error!(p, p.input.prev_span(), SyntaxError::ExpectedIdent)
                    panic!("ExpectedIdent at {:?}", p.input.prev_span());
                }
                // Word::Keyword(Keyword::This) if p.input.syntax().typescript() => {
                //     Ok(js_word!("this"))
                // }
                Word::Keyword(Keyword::Let) => js_word!("let"),
                Word::Ident(ident) => ident,
                Word::Keyword(Keyword::Yield) if incl_yield => js_word!("yield"),
                Word::Keyword(Keyword::Await) if incl_await => js_word!("await"),
                Word::Keyword(..) | Word::Null | Word::True | Word::False => {
                    // syntax_error!(p, p.input.prev_span(), SyntaxError::ExpectedIdent)
                    panic!("ExpectedIdent at {:?}", p.input.prev_span());
                }
            }
        });

        Ident::new(word, span!(self, start))
    }
}

pub(super) trait MaybeOptionalIdentParser<Ident> {
    fn parse_maybe_opt_binding_ident(&mut self) -> Ident;
}
impl<I: Tokens> MaybeOptionalIdentParser<Ident> for Parser<I> {
    fn parse_maybe_opt_binding_ident(&mut self) -> Ident {
        self.parse_binding_ident().id
    }
}
impl<I: Tokens> MaybeOptionalIdentParser<Option<Ident>> for Parser<I> {
    fn parse_maybe_opt_binding_ident(&mut self) -> Option<Ident> {
        self.parse_opt_binding_ident().map(|opt| opt.id)
    }
}
