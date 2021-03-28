// import type { ParsingError } from "../parser/error";

use crate::options::{Options, SourceType};

use crate::parser::ast::{Comment, Decorator, NodeBase};

use super::types::{TokenType, TokenTypes};

use super::context::TokContext;

use std::collections::HashMap;

use crate::parser::location::Position;

use super::TokenValue;

// type TopicContextState = {
//   // When a topic binding has been currently established,
//   // then this is 1. Otherwise, it is 0. This is forwards compatible
//   // with a future plugin for multiple lexical topics.
//   maxNumOfResolvableTopics: number,

//   // When a topic binding has been currently established, and if that binding
//   // has been used as a topic reference `#`, then this is 0. Otherwise, it is
//   // `null`. This is forwards compatible with a future plugin for multiple
//   // lexical topics.
//   maxTopicIndex: null | 0,
// };

#[derive(Clone)]
enum LabelKind {
    Loop,
    Switch,
}

#[derive(Clone)]
struct Label {
    kind: LabelKind,
    name: Option<String>,
    statement_start: Option<usize>,
}

#[derive(Clone)]
struct CommentStackElement {
    start: usize,
    leading_comments: Vec<Comment>,
    trailing_comments: Vec<Comment>,
    kind: String,
}

pub struct State<'a> {
    pub strict: bool,
    pub cur_line: usize,

    // And, if locations are used, the {line, column} object
    // corresponding to those offsets
    pub start_loc: Position,
    pub end_loc: Position,

    // errors: Vec<ParsingError>,

    // Used to signify the start of a potential arrow function
    potential_arrow_at: Option<usize>,

    // Used to signify the start of an expression which looks like a
    // typed arrow function, but it isn't
    // e.g. a ? (b) : c => d
    //          ^
    no_arrow_at: Vec<usize>,

    // Used to signify the start of an expression whose params, if it looks like
    // an arrow function, shouldn't be converted to assignable nodes.
    // This is used to defer the validation of typed arrow functions inside
    // conditional expressions.
    // e.g. a ? (b) : c => d
    //          ^
    no_arrow_params_conversion_at: Vec<usize>,

    // Flags to track
    maybe_in_arrow_parameters: bool,
    in_pipeline: bool,
    pub in_type: bool,
    no_anon_function_type: bool,
    in_property_name: bool,
    has_flow_comment: bool,
    pub is_iterator: bool,
    is_declare_context: bool,
    in_abstract_class: bool,

    // For the smartPipelines plugin:
    // topic_context: TopicContextState = {
    //   maxNumOfResolvableTopics: 0,
    //   maxTopicIndex: null,
    // },

    // For the F# plugin
    // solowait: bool = false,
    // inFSharpPipelineDirectBody: bool = false,

    // Labels in scope.
    labels: Vec<Label>,

    // Leading decorators. Last element of the stack represents the decorators in current context.
    // Supports nesting of decorators, e.g. @foo(@bar class inner {}) class outer {}
    // where @foo belongs to the outer class and @bar to the inner
    decorator_stack: Vec<Vec<Decorator>>,

    // Comment store.
    pub comments: Vec<Comment>,

    // Comment attachment store
    pub trailing_comments: Vec<Comment>,
    pub leading_comments: Vec<Comment>,
    comment_stack: Vec<CommentStackElement>,
    // $FlowIgnore this is initialized when the parser starts.
    comment_previous_node: Option<NodeBase>,

    // The current position of the tokenizer in the input.
    pub pos: usize,
    pub line_start: usize,

    // Properties of the current token:
    // Its type
    pub kind: &'a TokenType<'a>,

    // For tokens that include more information than their type, the value
    pub value: Option<TokenValue<'a>>,

    // Its start and end offset
    pub start: usize,
    pub end: usize,

    // Position information for the previous token
    // $FlowIgnore this is initialized when generating the second token.
    pub last_tok_end_loc: Option<Position>,
    // $FlowIgnore this is initialized when generating the second token.
    pub last_tok_start_loc: Option<Position>,
    pub last_tok_start: usize,
    pub last_tok_end: usize,

    // The context stack is used to superficially track syntactic
    // context to predict whether a regular expression is allowed in a
    // given position.
    pub context: Vec<TokContext>,
    pub expr_allowed: bool,

    // Used to signal to callers of `readWord1` whether the word
    // contained any escape sequences. This is needed because words with
    // escape sequences must not be interpreted as keywords.
    pub contains_esc: bool,

    // This property is used to track the following errors
    // - StrictNumericEscape
    // - StrictOctalLiteral
    //
    // in a literal that occurs prior to/immediately after a "use strict" directive.

    // todo(JLHwung): set strictErrors to null and avoid recording string errors
    // after a non-directive is parsed
    strict_errors: HashMap<usize, String>,

    // Names of exports store. `default` is stored as a name for both
    // `export default foo;` and `export { foo as default };`.
    exported_identifiers: Vec<String>,

    // Tokens length in token store
    pub tokens_length: usize,
}

impl<'a> State<'a> {
    pub fn new(options: &Options) -> Self {
        let strict = if options.strict_mode == Some(false) {
            false
        } else {
            options.source_type == SourceType::Module
        };

        Self {
            strict,
            cur_line: options.start_line,
            start_loc: Position {
                line: options.start_line,
                column: 0,
            },
            end_loc: Position {
                line: options.start_line,
                column: 0,
            },
            // errors: Vec::new(),
            potential_arrow_at: None,
            no_arrow_at: Vec::new(),
            no_arrow_params_conversion_at: Vec::new(),
            maybe_in_arrow_parameters: false,
            in_pipeline: false,
            in_type: false,
            no_anon_function_type: false,
            in_property_name: false,
            has_flow_comment: false,
            is_iterator: false,
            is_declare_context: false,
            in_abstract_class: false,
            labels: Vec::new(),
            decorator_stack: vec![Vec::new()],
            comments: Vec::new(),
            trailing_comments: Vec::new(),
            leading_comments: Vec::new(),
            comment_stack: Vec::new(),
            comment_previous_node: None,
            pos: 0,
            line_start: 0,
            kind: &TokenTypes::EOF,
            value: None,
            start: 0,
            end: 0,
            last_tok_end_loc: None,
            last_tok_start_loc: None,
            last_tok_start: 0,
            last_tok_end: 0,
            context: vec![TokContext::new_brace_statement_context()],
            expr_allowed: true,
            contains_esc: false,
            strict_errors: HashMap::new(),
            exported_identifiers: Vec::new(),
            tokens_length: 0,
        }
    }

    pub fn curPosition(&self) -> Position {
        Position {
            line: self.cur_line,
            column: self.pos - self.line_start,
        }
    }

    // pub fn clone(&self, skip_arrays: bool) -> Self {
    //     // let state = State::new(&self.options);
    //     // let state: State =State {
    //     //   strict: self.strict,
    //     //   cur_line: self.cur_line,
    //     //   start_loc: self.start_loc,
    //     //   end_loc: self.end_loc,
    //     //   // errors: Vec::new(),
    //     //   potential_arrow_at: self.potential_arrow_at,
    //     //   no_arrow_at: self.no_arrow_at,
    //     //   no_arrow_params_conversion_at: self.no_arrow_params_conversion_at,
    //     //   maybe_in_arrow_parameters: self.maybe_in_arrow_parameters,
    //     //   in_pipeline: self.in_pipeline,
    //     //   in_type: self.in_type,
    //     //   no_anon_function_type: self.no_anon_function_type,
    //     //   in_property_name: self.in_property_name,
    //     //   has_flow_comment: self.has_flow_comment,
    //     //   is_iterator: self.is_iterator,
    //     //   is_declare_context: self.is_declare_context,
    //     //   in_abstract_class: self.in_abstract_class,
    //     //   labels: self.labels,
    //     //   decorator_stack: self.decorator_stack,
    //     //   comments: self.comments,
    //     //   trailing_comments: self.trailing_comments,
    //     //   leading_comments: self.leading_comments,
    //     //   comment_stack: self.comment_stack,
    //     //   comment_previous_node: self.comment_previous_node,
    //     //   pos: self.pos,
    //     //   line_start: self.line_start,
    //     //   kind: self.kind,
    //     //   value: self.value,
    //     //   start: self.start,
    //     //   end: self.end,
    //     //   last_tok_end_loc: self.last_tok_end_loc,
    //     //   last_tok_start_loc: self.last_tok_start_loc,
    //     //   last_tok_start: self.last_tok_start,
    //     //   last_tok_end: self.last_tok_end,
    //     //   context: self.context,
    //     //   expr_allowed: self.expr_allowed,
    //     //   contains_esc: self.contains_esc,
    //     //   strict_errors: self.strict_errors,
    //     //   exported_identifiers: self.exported_identifiers,
    //     //   tokens_length: self.tokens_length,
    //     // };

    //     if skip_arrays {
    //         Self {
    //             start_loc: self.start_loc.clone(),
    //             end_loc: self.end_loc.clone(),
    //             ..*self
    //         }
    //     } else {
    //         Self {
    //             start_loc: self.start_loc.clone(),
    //             end_loc: self.end_loc.clone(),
    //             no_arrow_at: self.no_arrow_at.clone(),
    //             no_arrow_params_conversion_at: self.no_arrow_params_conversion_at.clone(),
    //             labels: self.labels.clone(),
    //             decorator_stack: vec![Vec::new()],
    //             comments: self.comments.clone(),
    //             trailing_comments: self.trailing_comments.clone(),
    //             leading_comments: self.leading_comments.clone(),
    //             comment_stack: self.comment_stack.clone(),

    //             context: vec![TokContext::new_brace_statement_context()],

    //             exported_identifiers: self.exported_identifiers.clone(),
    //             ..*self
    //         }
    //     }
    //     // const keys = Object.keys(this);
    //     // for (let i = 0, length = keys.length; i < length; i++) {
    //     //   const key = keys[i];
    //     //   // $FlowIgnore
    //     //   let val = this[key];

    //     //   if (!skip_arrays && Array.isArray(val)) {
    //     //     val = val.slice();
    //     //   }

    //     //   // $FlowIgnore
    //     //   state[key] = val;
    //     // }

    //     // return state;
    // }
}
