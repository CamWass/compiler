// import type { PluginList } from "./plugin-utils";

// A second optional argument can be given to further configure
// the parser process. These options are recognized:

#[derive(PartialEq)]
pub enum SourceType {
    Script,
    Module,
    Unambiguous,
}

pub struct Options {
    pub source_type: SourceType,
    pub source_filename: Option<String>,
    pub start_line: usize,
    pub allow_await_outside_function: bool,
    pub allow_return_outside_function: bool,
    pub allow_import_export_everywhere: bool,
    pub allow_super_outside_method: bool,
    pub allow_undeclared_exports: bool,
    // plugins: PluginList,
    pub strict_mode: Option<bool>,
    pub ranges: bool,
    pub tokens: bool,
    pub create_parenthesized_expressions: bool,
    pub error_recovery: bool,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            source_type: SourceType::Script,
            source_filename: None,
            start_line: 1,
            allow_await_outside_function: false,
            allow_return_outside_function: false,
            allow_import_export_everywhere: false,
            allow_super_outside_method: false,
            allow_undeclared_exports: false,
            // plugins: Vec::new(),
            strict_mode: None,
            ranges: false,
            tokens: false,
            create_parenthesized_expressions: false,
            error_recovery: false,
        }
    }
}

// export const defaultOptions: Options = {
//   // Source type ("script" or "module") for different semantics
//   source_type: "script",
//   // Source filename.
//   source_filename: undefined,
//   // Line from which to start counting source. Useful for
//   // integration with other tools.
//   start_line: 1,
//   // When enabled, await at the top level is not considered an
//   // error.
//   allow_await_outside_function: false,
//   // When enabled, a return at the top level is not considered an
//   // error.
//   allow_return_outside_function: false,
//   // When enabled, import/export statements are not constrained to
//   // appearing at the top of the program.
//   allow_import_export_everywhere: false,
//   // TODO
//   allow_super_outside_method: false,
//   // When enabled, export statements can reference undeclared variables.
//   allow_undeclared_exports: false,
//   // An array of plugins to enable
//   plugins: [],
//   // TODO
//   strict_mode: null,
//   // Nodes have their start and end characters offsets recorded in
//   // `start` and `end` properties (directly on the node, rather than
//   // the `loc` object, which holds line/column data. To also add a
//   // [semi-standardized][range] `range` property holding a `[start,
//   // end]` array with the same numbers, set the `ranges` option to
//   // `true`.
//   //
//   // [range]: https://bugzilla.mozilla.org/show_bug.cgi?id=745678
//   ranges: false,
//   // Adds all parsed tokens to a `tokens` property on the `File` node
//   tokens: false,
//   // Whether to create ParenthesizedExpression AST nodes (if false
//   // the parser sets extra.parenthesized on the expression nodes instead).
//   create_parenthesized_expressions: false,
//   // When enabled, errors are attached to the AST instead of being directly thrown.
//   // Some errors will still throw, because @babel/parser can't always recover.
//   error_recovery: false,
// };

// Interpret and default an options object

// export function getOptions(opts: ?Options): Options {
//   const options: any = {};
//   for (const key of Object.keys(defaultOptions)) {
//     options[key] = opts && opts[key] != null ? opts[key] : defaultOptions[key];
//   }
//   return options;
// }
