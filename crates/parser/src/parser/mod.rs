#[macro_use]
mod macros;
mod class_and_fn;
mod expression;
mod identifier;
mod input;
mod jsx;
mod object;
mod pat;
mod statement;
mod typescript;
mod util;

use crate::{
    context::{Context, YesMaybe, YesNoMaybe},
    error::{Error, SyntaxError},
    lexer::Lexer,
    token::{Token, Word},
    JscTarget, Syntax,
};
use ast2::*;
use context::AstContext;
use global_common::{BytePos, Span};
use input::Buffer;
pub use input::Tokens;
use std::{collections::HashMap, rc::Rc};
use swc_atoms::JsWord;

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
    trailing_commas_after_rest: HashMap<Span, Span>,
}

/// When error occurs, error is emitted and parser returns Err(()).
pub type PResult<T> = Result<T, Error>;

/// EcmaScript parser.
#[derive(Clone)]
pub struct Parser<'ast, I: Tokens> {
    /// [false] while backtracking
    emit_err: bool,
    state: State,
    input: Buffer<I>,
    ast: &'ast mut AstContext<'ast>,
}

impl<'ast, 'src> Parser<'ast, Lexer<'src>> {
    pub fn new(ast_context: &'ast mut AstContext<'ast>, syntax: Syntax, input: &'src str) -> Self {
        Self::new_from(ast_context, Lexer::new(syntax, Default::default(), input))
    }
}

impl<'ast, I: Tokens> Parser<'ast, I> {
    pub fn new_from(ast_context: &'ast mut AstContext<'ast>, input: I) -> Self {
        Parser {
            emit_err: true,
            state: Default::default(),
            input: Buffer::new(input),
            ast: ast_context,
        }
    }

    pub fn take_errors(&mut self) -> Vec<Error> {
        self.input().take_errors()
    }

    pub(crate) fn target(&self) -> JscTarget {
        self.input.target()
    }

    pub fn parse_script(&mut self) -> PResult<&'ast Script> {
        trace_cur!(self, parse_script);

        let ctx = Context {
            module: YesNoMaybe::No,
            ..self.ctx()
        };
        self.set_ctx(ctx);

        let start = self.input.cur_pos();

        let shebang = self.parse_shebang()?;

        self.parse_block_body(true, true, None).map(|body| {
            alloc!(
                self,
                Script {
                    span: span!(self, start),
                    body,
                    shebang,
                }
            )
        })
    }

    pub fn parse_typescript_module(&mut self) -> PResult<&'ast Module> {
        trace_cur!(self, parse_typescript_module);

        debug_assert!(self.syntax().typescript());

        //TODO (swc): parse() -> PResult<Program>
        let ctx = Context {
            module: YesNoMaybe::Yes,
            strict: YesMaybe::Yes,
            ..self.ctx()
        };
        // Module code is always in strict mode
        self.set_ctx(ctx);

        let start = self.input.cur_pos();
        let shebang = self.parse_shebang()?;

        self.parse_block_body(true, true, None).map(|body| {
            alloc!(
                self,
                Module {
                    span: span!(self, start),
                    body,
                    shebang,
                }
            )
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

            let script = alloc!(
                self,
                Script {
                    span: span!(self, start),
                    body,
                    shebang,
                }
            );

            Program::Script(script)
        } else {
            let module = alloc!(
                self,
                Module {
                    span: span!(self, start),
                    body,
                    shebang,
                }
            );

            Program::Module(module)
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

        self.parse_block_body(true, true, None).map(|body| {
            alloc!(
                self,
                Module {
                    span: span!(self, start),
                    body,
                    shebang,
                }
            )
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
