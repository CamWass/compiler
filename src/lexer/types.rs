// ## Token types

// The assignment of fine-grained, information-carrying type objects
// allows the tokenizer to store the information it has about a
// token in a way that is very cheap for the parser to look up.

// All token type variables start with an underscore, to make them
// easy to recognize.

// The `before_expr` property is used to disambiguate between regular
// expressions and divisions. It is set on all token types that can
// be followed by an expression (thus, a slash after them would be a
// regular expression).

// The `starts_expr` property is used to determine whether an expression
// may be the “argument” subexpression of a `yield` expression or
// `yield` statement. It is set on all token types that may be at the
// start of a subexpression.

// `is_loop` marks a keyword as starting a loop, which is important
// to know when parsing a label, in order to allow or disallow
// continue jumps to that label.

// #[derive(Default)]
// pub struct TokenOptions {
//     keyword: Option<&'static str>,
//     before_expr: bool,
//     starts_expr: bool,
//     right_associative: bool,
//     is_loop: bool,
//     is_assign: bool,
//     prefix: bool,
//     postfix: bool,
//     binop: Option<usize>,
// }

#[derive(PartialEq, Debug)]
pub struct TokenType<'a> {
    label: &'a str,
    pub keyword: Option<&'a str>,
    pub before_expr: bool,
    starts_expr: bool,
    right_associative: bool,
    is_loop: bool,
    is_assign: bool,
    prefix: bool,
    postfix: bool,
    binop: Option<usize>,
    // update_context: ?(prevType: TokenType) => void;
}

// impl TokenType {
//     pub fn new(label: &'static str, conf: &TokenOptions) -> Self {
//         Self {
//             // label: String::from(label),
//             label,
//             keyword: conf.keyword,
//             before_expr: conf.before_expr,
//             starts_expr: conf.starts_expr,
//             right_associative: conf.right_associative,
//             is_loop: conf.is_loop,
//             is_assign: conf.is_assign,
//             prefix: conf.prefix,
//             postfix: conf.postfix,
//             binop: conf.binop,
//             // this.update_context = null;
//         }
//     }
// }

pub mod TokenTypes {
    use super::*;

    pub static NUM: TokenType = TokenType {
        label: "num",
        keyword: None,
        before_expr: false,
        starts_expr: true,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static BIG_INT: TokenType = TokenType {
        label: "bigint",
        keyword: None,
        before_expr: false,
        starts_expr: true,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    // pub static decimal: TokenType = TokenType::new(
    //     "decimal",
    //     &TokenOptions {
    //         starts_expr,
    //         ..Default::default()
    //     },
    // );
    pub static REGEXP: TokenType = TokenType {
        label: "regexp",
        keyword: None,
        before_expr: false,
        starts_expr: true,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static STRING: TokenType = TokenType {
        label: "string",
        keyword: None,
        before_expr: false,
        starts_expr: true,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static NAME: TokenType = TokenType {
        label: "name",
        keyword: None,
        before_expr: false,
        starts_expr: true,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static EOF: TokenType = TokenType {
        label: "eof",
        keyword: None,
        before_expr: false,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };

    // Punctuation token types.
    pub static BRACKET_L: TokenType = TokenType {
        label: "[",
        keyword: None,
        before_expr: true,
        starts_expr: true,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    // pub static bracketHashL: TokenType = TokenType::new(
    //     "#[",
    //     &TokenOptions {
    //         before_expr,
    //         starts_expr,
    //         ..Default::default()
    //     },
    // );
    // pub static bracketBarL: TokenType = TokenType::new(
    //     "[|",
    //     &TokenOptions {
    //         before_expr,
    //         starts_expr,
    //         ..Default::default()
    //     },
    // );
    pub static BRACKET_R: TokenType = TokenType {
        label: "]",
        keyword: None,
        before_expr: false,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    // pub static bracketBarR: TokenType = TokenType::new("|]", &Default::default());
    pub static BRACE_L: TokenType = TokenType {
        label: "{",
        keyword: None,
        before_expr: true,
        starts_expr: true,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    // pub static braceBarL: TokenType = TokenType::new(
    //     "{|",
    //     &TokenOptions {
    //         before_expr,
    //         starts_expr,
    //         ..Default::default()
    //     },
    // );
    // pub static braceHashL: TokenType = TokenType::new(
    //     "#{",
    //     &TokenOptions {
    //         before_expr,
    //         starts_expr,
    //         ..Default::default()
    //     },
    // );
    pub static BRACE_R: TokenType = TokenType {
        label: "}",
        keyword: None,
        before_expr: false,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    // pub static braceBarR: TokenType = TokenType::new("|}", &Default::default());
    pub static PAREN_L: TokenType = TokenType {
        label: "(",
        keyword: None,
        before_expr: true,
        starts_expr: true,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static PAREN_R: TokenType = TokenType {
        label: ")",
        keyword: None,
        before_expr: false,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static COMMA: TokenType = TokenType {
        label: ",",
        keyword: None,
        before_expr: true,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static SEMI: TokenType = TokenType {
        label: ";",
        keyword: None,
        before_expr: true,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static COLON: TokenType = TokenType {
        label: ":",
        keyword: None,
        before_expr: true,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    // pub static doubleColon: TokenType = TokenType {
    //     label: "::",
    //     keyword: None,
    //     before_expr: true,
    //     starts_expr: false,
    //     right_associative: false,
    //     is_loop: false,
    //     is_assign: false,
    //     prefix: false,
    //     postfix: false,
    //     binop: None,
    // };
    pub static DOT: TokenType = TokenType {
        label: ".",
        keyword: None,
        before_expr: false,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static QUESTION: TokenType = TokenType {
        label: "?",
        keyword: None,
        before_expr: true,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static QUESTION_DOT: TokenType = TokenType {
        label: "?.",
        keyword: None,
        before_expr: false,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static ARROW: TokenType = TokenType {
        label: "=>",
        keyword: None,
        before_expr: true,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    // pub static template: TokenType = TokenType {
    //     label: "template",
    //     keyword: None,
    //     before_expr: false,
    //     starts_expr: false,
    //     right_associative: false,
    //     is_loop: false,
    //     is_assign: false,
    //     prefix: false,
    //     postfix: false,
    //     binop: None,
    // };
    pub static ELLIPSIS: TokenType = TokenType {
        label: "...",
        keyword: None,
        before_expr: true,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static BACK_QUOTE: TokenType = TokenType {
        label: "`",
        keyword: None,
        before_expr: false,
        starts_expr: true,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    // pub static dollarBraceL: TokenType = TokenType::new(
    //     "${",
    //     &TokenOptions {
    //         before_expr,
    //         starts_expr,
    //         ..Default::default()
    //     },
    // );
    pub static AT: TokenType = TokenType {
        label: "@",
        keyword: None,
        before_expr: false,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static HASH: TokenType = TokenType {
        label: "#",
        keyword: None,
        before_expr: false,
        starts_expr: true,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };

    // Special hashbang token.
    pub static INTERPRETER_DIRECTIVE: TokenType = TokenType {
        label: "#!...",
        keyword: None,
        before_expr: false,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };

    // Operators. These carry several kinds of properties to help the
    // parser use them properly (the presence of these properties is
    // what categorizes them as operators).
    //
    // `binop`, when present, specifies that this operator is a binary
    // operator, and will refer to its precedence.
    //
    // `prefix` and `postfix` mark the operator as a prefix or postfix
    // unary operator.
    //
    // `isAssign` marks all of `=`, `+=`, `-=` etcetera, which act as
    // binary operators with a very low precedence, that should result
    // in AssignmentExpression nodes.

    pub static EQ: TokenType = TokenType {
        label: "=",
        keyword: None,
        before_expr: true,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: true,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static ASSIGN: TokenType = TokenType {
        label: "_=",
        keyword: None,
        before_expr: true,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: true,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static INC_DEC: TokenType = TokenType {
        label: "++/--",
        keyword: None,
        before_expr: false,
        starts_expr: true,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: true,
        postfix: true,
        binop: None,
    };
    pub static BANG: TokenType = TokenType {
        label: "!",
        keyword: None,
        before_expr: true,
        starts_expr: true,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: true,
        postfix: false,
        binop: None,
    };
    pub static TILDE: TokenType = TokenType {
        label: "~",
        keyword: None,
        before_expr: true,
        starts_expr: true,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: true,
        postfix: false,
        binop: None,
    };
    pub static PIPELINE: TokenType = TokenType {
        label: "|>",
        keyword: None,
        before_expr: true,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: Some(0),
    };
    pub static NULLISH_COALESCING: TokenType = TokenType {
        label: "??",
        keyword: None,
        before_expr: true,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: Some(1),
    };
    pub static LOGICAL_OR: TokenType = TokenType {
        label: "||",
        keyword: None,
        before_expr: true,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: Some(1),
    };
    pub static LOGICAL_AND: TokenType = TokenType {
        label: "&&",
        keyword: None,
        before_expr: true,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: Some(2),
    };
    pub static BITWISE_OR: TokenType = TokenType {
        label: "|",
        keyword: None,
        before_expr: true,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: Some(3),
    };
    pub static BITWISE_XOR: TokenType = TokenType {
        label: "^",
        keyword: None,
        before_expr: true,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: Some(4),
    };
    pub static BITWISE_AND: TokenType = TokenType {
        label: "&",
        keyword: None,
        before_expr: true,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: Some(5),
    };
    pub static EQUALITY: TokenType = TokenType {
        label: "==/!=/===/!==",
        keyword: None,
        before_expr: true,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: Some(6),
    };
    pub static RELATIONAL: TokenType = TokenType {
        label: "</>/<=/>=",
        keyword: None,
        before_expr: true,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: Some(7),
    };
    pub static BIT_SHIFT: TokenType = TokenType {
        label: "<</>>/>>>",
        keyword: None,
        before_expr: true,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: Some(8),
    };
    pub static PLUS_MIN: TokenType = TokenType {
        label: "+/-",
        keyword: None,
        before_expr: true,
        starts_expr: true,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: true,
        postfix: false,
        binop: Some(9),
    };
    // starts_expr: required by v8intrinsic plugin
    pub static MODULO: TokenType = TokenType {
        label: "%",
        keyword: None,
        before_expr: true,
        starts_expr: true,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: Some(10),
    };
    // unset `before_expr` as it can be `function *`
    pub static STAR: TokenType = TokenType {
        label: "*",
        keyword: None,
        before_expr: false,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: Some(10),
    };
    pub static SLASH: TokenType = TokenType {
        label: "/",
        keyword: None,
        before_expr: true,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: Some(10),
    };
    pub static EXPONENT: TokenType = TokenType {
        label: "**",
        keyword: None,
        before_expr: true,
        starts_expr: false,
        right_associative: true,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: Some(11),
    };

    // Keywords
    pub static KW_BREAK: TokenType = TokenType {
        label: "break",
        keyword: Some("break"),
        before_expr: false,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static KW_CASE: TokenType = TokenType {
        label: "case",
        keyword: Some("case"),
        before_expr: true,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static KW_CATCH: TokenType = TokenType {
        label: "catch",
        keyword: Some("catch"),
        before_expr: false,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static KW_CONTINUE: TokenType = TokenType {
        label: "continue",
        keyword: Some("continue"),
        before_expr: false,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static KW_DEBUGGER: TokenType = TokenType {
        label: "debugger",
        keyword: Some("debugger"),
        before_expr: false,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static KW_DEFAULT: TokenType = TokenType {
        label: "default",
        keyword: Some("default"),
        before_expr: true,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static KW_DO: TokenType = TokenType {
        label: "do",
        keyword: Some("do"),
        before_expr: true,
        starts_expr: false,
        right_associative: false,
        is_loop: true,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static KW_ELSE: TokenType = TokenType {
        label: "else",
        keyword: Some("else"),
        before_expr: true,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static KW_FINALLY: TokenType = TokenType {
        label: "finally",
        keyword: Some("finally"),
        before_expr: false,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static KW_FOR: TokenType = TokenType {
        label: "for",
        keyword: Some("for"),
        before_expr: false,
        starts_expr: false,
        right_associative: false,
        is_loop: true,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static KW_FUNCTION: TokenType = TokenType {
        label: "function",
        keyword: Some("function"),
        before_expr: false,
        starts_expr: true,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static KW_IF: TokenType = TokenType {
        label: "if",
        keyword: Some("if"),
        before_expr: false,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static KW_RETURN: TokenType = TokenType {
        label: "return",
        keyword: Some("return"),
        before_expr: true,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static KW_SWITCH: TokenType = TokenType {
        label: "switch",
        keyword: Some("switch"),
        before_expr: false,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static KW_THROW: TokenType = TokenType {
        label: "throw",
        keyword: Some("throw"),
        before_expr: true,
        starts_expr: true,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: true,
        postfix: false,
        binop: None,
    };
    pub static KW_TRY: TokenType = TokenType {
        label: "try",
        keyword: Some("try"),
        before_expr: false,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static KW_VAR: TokenType = TokenType {
        label: "var",
        keyword: Some("var"),
        before_expr: false,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static KW_CONST: TokenType = TokenType {
        label: "const",
        keyword: Some("const"),
        before_expr: false,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static KW_WHILE: TokenType = TokenType {
        label: "while",
        keyword: Some("while"),
        before_expr: false,
        starts_expr: false,
        right_associative: false,
        is_loop: true,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static KW_WITH: TokenType = TokenType {
        label: "with",
        keyword: Some("with"),
        before_expr: false,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static KW_NEW: TokenType = TokenType {
        label: "new",
        keyword: Some("new"),
        before_expr: true,
        starts_expr: true,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static KW_THIS: TokenType = TokenType {
        label: "this",
        keyword: Some("this"),
        before_expr: false,
        starts_expr: true,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static KW_SUPER: TokenType = TokenType {
        label: "super",
        keyword: Some("super"),
        before_expr: false,
        starts_expr: true,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static KW_CLASS: TokenType = TokenType {
        label: "class",
        keyword: Some("class"),
        before_expr: false,
        starts_expr: true,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static KW_EXTENDS: TokenType = TokenType {
        label: "extends",
        keyword: Some("extends"),
        before_expr: true,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static KW_EXPORT: TokenType = TokenType {
        label: "export",
        keyword: Some("export"),
        before_expr: false,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static KW_IMPORT: TokenType = TokenType {
        label: "import",
        keyword: Some("import"),
        before_expr: false,
        starts_expr: true,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static KW_NULL: TokenType = TokenType {
        label: "null",
        keyword: Some("null"),
        before_expr: false,
        starts_expr: true,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static KW_TRUE: TokenType = TokenType {
        label: "true",
        keyword: Some("true"),
        before_expr: false,
        starts_expr: true,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static KW_FALSE: TokenType = TokenType {
        label: "false",
        keyword: Some("false"),
        before_expr: false,
        starts_expr: true,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: None,
    };
    pub static KW_IN: TokenType = TokenType {
        label: "in",
        keyword: Some("in"),
        before_expr: true,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: Some(7),
    };
    pub static KW_INSTANCEOF: TokenType = TokenType {
        label: "instanceof",
        keyword: Some("instanceof"),
        before_expr: true,
        starts_expr: false,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: false,
        postfix: false,
        binop: Some(7),
    };
    pub static KW_TYPEOF: TokenType = TokenType {
        label: "typeof",

        keyword: Some("typeof"),
        before_expr: true,
        starts_expr: true,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: true,
        postfix: false,
        binop: None,
    };
    pub static KW_VOID: TokenType = TokenType {
        label: "void",
        keyword: Some("void"),
        before_expr: true,
        starts_expr: true,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: true,
        postfix: false,
        binop: None,
    };
    pub static KW_DELETE: TokenType = TokenType {
        label: "delete",
        keyword: Some("delete"),
        before_expr: true,
        starts_expr: true,
        right_associative: false,
        is_loop: false,
        is_assign: false,
        prefix: true,
        postfix: false,
        binop: None,
    };
}
