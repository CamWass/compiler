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
    context::{Context, YesMaybe, YesNoMaybe},
    error::{Error, SyntaxError},
    lexer::Lexer,
    token::{Token, Word},
    JscTarget, Syntax,
};
use ast::*;
use atoms::JsWord;
use global_common::{BytePos, SourceFile, Span};
use input::Buffer;
pub use input::Tokens;
use rustc_hash::{FxHashMap, FxHashSet};
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone, Default)]
struct State {
    labels: Vec<JsWord>,
    /// Start position of an assignment expression.
    potential_arrow_start: Option<BytePos>,
    /// Tracks the positions of commas that directly follow spread elements in arrays.
    ///
    /// For example: `[...a,]`
    ///
    /// Only tracks the first matching comma in an array.
    trailing_commas_after_rest: FxHashMap<Span, Span>,
    parenthesised_exprs: FxHashSet<NodeId>,
}

/// When error occurs, error is emitted and parser returns Err(()).
pub type PResult<T> = Result<T, Error>;

/// EcmaScript parser.
#[derive(Clone)]
pub struct Parser<I: Tokens> {
    /// [false] while backtracking
    emit_err: bool,
    state: State,
    input: Buffer<I>,
    program_data: Rc<RefCell<ProgramData>>,
}

impl<'src> Parser<Lexer<'src>> {
    pub fn new(
        syntax: Syntax,
        input: &'src SourceFile,
        program_data: Rc<RefCell<ProgramData>>,
    ) -> Self {
        Self::new_from(Lexer::new(syntax, Default::default(), input), program_data)
    }
}

impl<I: Tokens> Parser<I> {
    pub fn new_from(input: I, program_data: Rc<RefCell<ProgramData>>) -> Self {
        Parser {
            emit_err: true,
            state: Default::default(),
            input: Buffer::new(input),
            program_data,
        }
    }

    pub fn take_errors(&mut self) -> Vec<Error> {
        self.input().take_errors()
    }

    pub(crate) fn target(&self) -> JscTarget {
        self.input.target()
    }

    pub fn parse_script(&mut self) -> PResult<Script> {
        trace_cur!(self, parse_script);

        let ctx = Context {
            module: YesNoMaybe::No,
            ..self.ctx()
        };
        self.set_ctx(ctx);

        let start = self.input.cur_pos();

        let shebang = self.parse_shebang()?;

        self.parse_block_body(true, true, None).map(|body| Script {
            node_id: node_id!(self, span!(self, start)),
            body,
            shebang,
        })
    }

    /// Returns [Module] if it's a module and returns [Script] if it's not a
    /// module.
    pub fn parse_program(&mut self) -> PResult<Program> {
        let start = self.input.cur_pos();
        let shebang = self.parse_shebang()?;

        let body: Vec<ModuleItem> = self.parse_block_body(true, true, None)?;

        Ok(if !self.ctx().is_module() {
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
                shebang,
            })
        } else {
            Program::Module(Module {
                node_id: node_id!(self, span!(self, start)),
                body,
                shebang,
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
        let shebang = self.parse_shebang()?;

        self.parse_block_body(true, true, None).map(|body| Module {
            node_id: node_id!(self, span!(self, start)),
            body,
            shebang,
        })
    }

    fn parse_shebang(&mut self) -> PResult<Option<JsWord>> {
        match self.input.cur() {
            Some(Token::Shebang(..)) => match self.input.bump() {
                Token::Shebang(v) => Ok(Some(v)),
                _ => unreachable!(),
            },
            _ => Ok(None),
        }
    }

    fn ctx(&self) -> Context {
        self.input.get_ctx()
    }

    #[cold]
    fn emit_err(&self, span: Span, error: SyntaxError) {
        if !self.emit_err || !self.syntax().early_errors() {
            return;
        }

        self.emit_error(Error {
            error: Box::new((span, error)),
        })
    }

    #[cold]
    fn emit_error(&self, error: Error) {
        if !self.emit_err || !self.syntax().early_errors() {
            return;
        }

        self.input_ref().add_error(error);
    }

    #[cold]
    fn emit_strict_mode_err(&self, span: Span, error: SyntaxError) {
        if !self.emit_err {
            return;
        }
        let error = Error {
            error: Box::new((span, error)),
        };
        self.input_ref().add_strict_mode_error(error);
    }
}
