#[macro_use]
mod macros;
mod class_and_fn;
mod expression;
mod identifier;
mod input;
mod object;
mod pat;
mod statement;
pub mod strip;
mod typescript;
mod util;

use crate::{
    JscTarget, Syntax,
    context::{Context, YesMaybe, YesNoMaybe},
    error::{Error, SyntaxError},
    lexer::Lexer,
    token::Token,
};
use ast::*;
use common::{BytePos, SourceFile, Span};
use input::Buffer;
use rustc_hash::{FxHashMap, FxHashSet};

/// When error occurs, error is emitted and parser returns Err(()).
pub type PResult<T> = Result<T, Error>;

/// EcmaScript parser.
pub struct Parser<'d> {
    /// [false] while backtracking
    emit_err: bool,
    input: Buffer<'d>,

    labels: Vec<NameId>,
    /// Start position of an assignment expression.
    potential_arrow_start: Option<BytePos>,
    /// Tracks the positions of commas that directly follow spread elements in arrays.
    ///
    /// For example: `[...a,]`
    ///
    /// Only tracks the first matching comma in an array.
    trailing_commas_after_rest: FxHashMap<NodeId, Span>,
    parenthesised_exprs: FxHashSet<NodeId>,
}

impl<'d> Parser<'d> {
    pub fn new(syntax: Syntax, input: &'d SourceFile, program_data: &'d mut ProgramData) -> Self {
        let input = Lexer::new(syntax, Default::default(), input, program_data);

        let start_pos = input.start_pos();

        let mut parser = Parser {
            emit_err: true,
            input: Buffer::new(input),

            labels: Vec::new(),
            potential_arrow_start: None,
            trailing_commas_after_rest: FxHashMap::default(),
            parenthesised_exprs: FxHashSet::default(),
        };

        // Skip shebang and initialise the parser with the first useful token.
        parser.input.first_bump();

        // This is a workaround for when the file contains only
        // whitespace/comments.
        if parser.input.cur.token == Token::Eof {
            parser.input.cur.span = Span::new(start_pos, start_pos);
        }

        parser
    }

    pub fn take_errors(&mut self) -> Vec<Error> {
        self.input().take_errors()
    }

    pub(crate) fn target(&self) -> JscTarget {
        self.input.target()
    }

    pub fn parse_script(&mut self) -> PResult<Script> {
        let ctx = Context {
            module: YesNoMaybe::No,
            ..self.ctx()
        };
        self.set_ctx(ctx);

        let start = self.input.cur_pos();

        self.parse_block_body(true, true, None).map(|body| Script {
            node_id: node_id!(self, self.span(start)),
            body,
        })
    }

    /// Returns [Module] if it's a module and returns [Script] if it's not a
    /// module.
    pub fn parse_program(&mut self) -> PResult<Program> {
        let start = self.input.cur_pos();

        let body: Vec<ModuleItem> = self.parse_block_body(true, true, None)?;

        Ok(if self.ctx().is_module() {
            Program::Module(Module {
                node_id: node_id!(self, self.span(start)),
                body,
            })
        } else {
            let body = body
                .into_iter()
                .map(|item| match item {
                    ModuleItem::ModuleDecl(_) => unreachable!("Module is handled above"),
                    ModuleItem::Stmt(stmt) => stmt,
                })
                .collect();
            Program::Script(Script {
                node_id: node_id!(self, self.span(start)),
                body,
            })
        })
    }

    pub fn parse_module(&mut self) -> PResult<Module> {
        let ctx = Context {
            module: YesNoMaybe::Yes,
            strict: YesMaybe::Yes,
            ..self.ctx()
        };
        // Module code is always in strict mode
        self.set_ctx(ctx);

        let start = self.input.cur_pos();

        self.parse_block_body(true, true, None).map(|body| Module {
            node_id: node_id!(self, self.span(start)),
            body,
        })
    }

    fn ctx(&self) -> Context {
        self.input.get_ctx()
    }

    #[cold]
    fn emit_err(&mut self, span: Span, error: SyntaxError) {
        if !self.emit_err || !self.syntax().early_errors() {
            return;
        }

        self.emit_error(Error {
            error: Box::new((span, error)),
        });
    }

    #[cold]
    fn emit_error(&mut self, error: Error) {
        if !self.emit_err || !self.syntax().early_errors() {
            return;
        }

        if let Token::Error = self.input.cur() {
            let error = self.input.expect_error_token_and_bump();
            self.input().add_error(error);
        }

        self.input().add_error(error);
    }

    #[cold]
    fn emit_strict_mode_err(&mut self, span: Span, error: SyntaxError) {
        if !self.emit_err {
            return;
        }
        let error = Error {
            error: Box::new((span, error)),
        };
        self.input().add_strict_mode_error(error);
    }

    /// Handles automatic semicolon insertion.
    fn is_semi_with_asi(&mut self) -> bool {
        match self.input.cur() {
            tok!('}') | Token::Semi | Token::Eof => true,
            _ => self.input.had_line_break_before_cur(),
        }
    }

    /// Handles automatic semicolon insertion.
    fn eat_semi_with_asi(&mut self) -> bool {
        match self.input.cur() {
            Token::Semi => {
                self.input.bump();
                true
            }
            tok!('}') | Token::Eof => true,
            _ => self.input.had_line_break_before_cur(),
        }
    }

    /// Handles automatic semicolon insertion.
    fn expect_semi_with_asi(&mut self) -> PResult<()> {
        if !self.eat_semi_with_asi() {
            let span = self.input.cur_span();
            let cur = self.input.dump_cur();
            syntax_error!(self, span, SyntaxError::Expected(tok!(';'), cur))
        }
        Ok(())
    }

    fn is_ident_ref(&mut self) -> bool {
        let ctx = self.ctx();
        self.input.cur().is_word() && !self.input.cur().is_reserved_word(ctx)
    }

    fn peek_is_ident_ref(&mut self) -> bool {
        let ctx = self.ctx();

        match self.input.peek() {
            Some(t) => t.is_word() && !t.is_reserved_word(ctx),
            _ => false,
        }
    }

    fn eat_ident_ref(&mut self) -> bool {
        if self.is_ident_ref() {
            self.input.bump();
            true
        } else {
            false
        }
    }

    fn is(&mut self, expected: Token) -> bool {
        self.input.is(expected)
    }

    fn peeked_is(&mut self, expected: Token) -> bool {
        self.input.peeked_is(expected)
    }

    fn eat(&mut self, expected: Token) -> bool {
        self.input.eat(expected)
    }

    fn span(&self, start: BytePos) -> Span {
        let end = self.input.prev_span().hi;
        debug_assert!(
            start <= end,
            "assertion failed: (span.start <= span.end). start = {start:?}, end = {end:?}",
        );
        Span::new(start, end)
    }

    #[cold]
    #[inline(never)]
    pub fn eof_error(&mut self) -> Error {
        debug_assert!(
            self.input.cur() == Token::Eof,
            "Parser should not call eof_error() without knowing current token"
        );
        let pos = self.input.cur_span().hi;
        let last = Span { lo: pos, hi: pos };
        Error {
            error: Box::new((last, SyntaxError::Eof)),
        }
    }
}
