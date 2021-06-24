#[macro_use]
mod macros;
mod class_and_fn;
mod expression;
mod identifier;
mod input;
mod object;
mod pat;
mod statement;
mod util;

use crate::{
    context::{Context, YesMaybe, YesNoMaybe},
    error::{Error, SyntaxError},
    lexer::Lexer,
    token::Token,
    JscTarget,
};
use ast::*;
use global_common::{input::StringInput, BytePos, Span};
use input::Buffer;
use std::collections::HashMap;
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

pub struct Parser {
    state: State,
    input: Buffer,
    ctx: Context,
    target: JscTarget,

    /// Buffer for errors that are invalid in all contexts.
    errors: Vec<Error>,
    /// Buffer for errors that are only invalid in module code.
    module_errors: Vec<Error>,
    /// Buffer for errors that are invalid in strict mode.
    strict_errors: Vec<Error>,
}

impl Parser {
    pub fn new(input: StringInput) -> Self {
        let mut errors = Vec::new();
        let mut module_errors = Vec::new();
        let mut strict_errors = Vec::new();

        let mk_error = |span, kind| Error {
            error: Box::new((span, kind)),
        };

        let mut lexer = Lexer::new(
            Default::default(),
            input,
            |span, kind| {
                errors.push(mk_error(span, kind));
            },
            |span, kind| {
                module_errors.push(mk_error(span, kind));
            },
            |span, kind| {
                strict_errors.push(mk_error(span, kind));
            },
        );

        let mut tokens = Vec::new();

        while let Some(token_and_span) = lexer.next() {
            match token_and_span.token {
                Token::Error(..) => {
                    tokens.push(token_and_span);
                    break;
                }
                _ => {
                    tokens.push(token_and_span);
                }
            }
        }

        Parser {
            state: Default::default(),
            input: Buffer::new(tokens),
            ctx: Default::default(),
            target: Default::default(),

            errors,
            module_errors,
            strict_errors,
        }
    }

    pub fn parse_script(&mut self) -> PResult<Script> {
        trace_cur!(self, parse_script);

        let ctx = Context {
            module: YesNoMaybe::No,
            ..self.ctx
        };
        self.set_ctx(ctx);

        let start = self.input.cur_pos();

        let shebang = self.parse_shebang()?;

        self.parse_block_body(true, true, None).map(|body| Script {
            span: span!(self, start),
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

        Ok(if !self.ctx.is_module() {
            let body = body
                .into_iter()
                .map(|item| match item {
                    ModuleItem::ModuleDecl(_) => unreachable!("Module is handled above"),
                    ModuleItem::Stmt(stmt) => stmt,
                })
                .collect();
            Program::Script(Script {
                span: span!(self, start),
                body,
                shebang,
            })
        } else {
            Program::Module(Module {
                span: span!(self, start),
                body,
                shebang,
            })
        })
    }

    pub fn parse_module(&mut self) -> PResult<Module> {
        let ctx = Context {
            module: YesNoMaybe::Yes,
            strict: YesMaybe::Yes,
            ..self.ctx
        };
        // Module code is always in strict mode
        self.set_ctx(ctx);

        let start = self.input.cur_pos();
        let shebang = self.parse_shebang()?;

        self.parse_block_body(true, true, None).map(|body| Module {
            span: span!(self, start),
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
}
