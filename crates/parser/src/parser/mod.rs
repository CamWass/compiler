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
    token::{Token, Word},
};
use ast::*;
use atoms::JsWord;
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
    program_data: &'d mut ProgramData,

    labels: Vec<JsWord>,
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
        let input = Lexer::new(syntax, Default::default(), input);

        Parser {
            emit_err: true,
            input: Buffer::new(input),
            program_data: program_data,

            labels: Vec::new(),
            potential_arrow_start: None,
            trailing_commas_after_rest: FxHashMap::default(),
            parenthesised_exprs: FxHashSet::default(),
        }
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
            node_id: node_id!(self, span!(self, start)),
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
                node_id: node_id!(self, span!(self, start)),
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
                node_id: node_id!(self, span!(self, start)),
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
            node_id: node_id!(self, span!(self, start)),
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
}
