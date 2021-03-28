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

// export const keywords = new Map<string, TokenType>();

pub mod TokenTypes {
    use super::*;

    pub static d: TokenType = TokenType {
        label: "foo",
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

    pub static num: TokenType = TokenType {
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
    pub static bigint: TokenType = TokenType {
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
    pub static regexp: TokenType = TokenType {
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
    pub static string: TokenType = TokenType {
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
    pub static name: TokenType = TokenType {
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
    pub static eof: TokenType = TokenType {
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
    pub static bracketL: TokenType = TokenType {
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
    pub static bracketR: TokenType = TokenType {
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
    pub static braceL: TokenType = TokenType {
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
    pub static braceR: TokenType = TokenType {
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
    pub static parenL: TokenType = TokenType {
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
    pub static parenR: TokenType = TokenType {
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
    pub static comma: TokenType = TokenType {
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
    pub static semi: TokenType = TokenType {
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
    pub static colon: TokenType = TokenType {
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
    pub static doubleColon: TokenType = TokenType {
        label: "::",
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
    pub static dot: TokenType = TokenType {
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
    pub static question: TokenType = TokenType {
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
    pub static questionDot: TokenType = TokenType {
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
    pub static arrow: TokenType = TokenType {
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
    pub static template: TokenType = TokenType {
        label: "template",
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
    pub static ellipsis: TokenType = TokenType {
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
    pub static backQuote: TokenType = TokenType {
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
    pub static at: TokenType = TokenType {
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
    pub static hash: TokenType = TokenType {
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
    pub static interpreterDirective: TokenType = TokenType {
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

    // TODO: temp
    // fn createBinop(name: string, binop: number) {
    //   return new TokenType(name, { beforeExpr, binop });
    // }

    pub static eq: TokenType = TokenType {
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
    pub static assign: TokenType = TokenType {
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
    pub static incDec: TokenType = TokenType {
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
    pub static bang: TokenType = TokenType {
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
    pub static tilde: TokenType = TokenType {
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
    pub static pipeline: TokenType = TokenType {
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
    pub static nullishCoalescing: TokenType = TokenType {
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
    pub static logicalOR: TokenType = TokenType {
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
    pub static logicalAND: TokenType = TokenType {
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
    pub static bitwiseOR: TokenType = TokenType {
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
    pub static bitwiseXOR: TokenType = TokenType {
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
    pub static bitwiseAND: TokenType = TokenType {
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
    pub static equality: TokenType = TokenType {
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
    pub static relational: TokenType = TokenType {
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
    pub static bitShift: TokenType = TokenType {
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
    pub static plusMin: TokenType = TokenType {
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
    pub static modulo: TokenType = TokenType {
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
    pub static star: TokenType = TokenType {
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
    pub static slash: TokenType = TokenType {
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
    pub static exponent: TokenType = TokenType {
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

    //   function createKeyword(name: string, options: TokenOptions = {}): TokenType {
    //   options.keyword = name;
    //   static token = new TokenType(name, options);
    //   keywords.set(name, token);
    //   return token;
    // }

    // Keywords
    // TODO: remove comment
    // Don't forget to update packages/babel-helper-validator-identifier/src/keyword.js
    // when new keywords are added
pub static KW_break: TokenType = TokenType{
        label:"break",
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
pub static KW_case: TokenType = TokenType{
        label:"case",
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
pub static KW_catch: TokenType = TokenType{
        label:"catch",
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
pub static KW_continue: TokenType = TokenType{
        label:"continue",
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
pub static KW_debugger: TokenType = TokenType{
        label:"debugger",
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
pub static KW_default: TokenType = TokenType{
        label:"default",
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
pub static KW_do: TokenType = TokenType{
        label:"do",
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
pub static KW_else: TokenType = TokenType{
        label:"else",
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
pub static KW_finally: TokenType = TokenType{
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
pub static KW_for: TokenType = TokenType{
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
pub static KW_function: TokenType = TokenType{
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
pub static KW_if: TokenType = TokenType{
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
pub static KW_return: TokenType = TokenType{
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
pub static KW_switch: TokenType = TokenType{
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
pub static KW_throw: TokenType = TokenType{
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
pub static KW_try: TokenType = TokenType{
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
pub static KW_var: TokenType = TokenType{
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
pub static KW_const: TokenType = TokenType{
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
pub static KW_while: TokenType = TokenType{
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
pub static KW_with: TokenType = TokenType{
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
pub static KW_new: TokenType = TokenType{
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
pub static KW_this: TokenType = TokenType{
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
pub static KW_super: TokenType = TokenType{
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
pub static KW_class: TokenType = TokenType{
      label:  "class",
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
pub static KW_extends: TokenType = TokenType{
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
pub static KW_export: TokenType = TokenType{
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
pub static KW_import: TokenType = TokenType{
      label:  "import",
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
pub static KW_null: TokenType = TokenType{
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
pub static KW_true: TokenType = TokenType{
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
pub static KW_false: TokenType = TokenType{
      label:  "false",
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
pub static KW_in: TokenType = TokenType{
     label:   "in",
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
pub static KW_instanceof: TokenType = TokenType{
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
pub static KW_typeof: TokenType = TokenType{
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
pub static KW_void: TokenType = TokenType{
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
pub static KW_delete: TokenType = TokenType{
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

    // pub fn get_keyword_kind<'a>(keyword: &'a String) -> Option<&'a TokenType<'a>> {
    //     match keyword.as_ref() {
    //         "break" => Some(&KW_break),
    //         "case" => Some(&KW_case),
    //         "catch" => Some(&KW_catch),
    //         "continue" => Some(&KW_continue),
    //         "debugger" => Some(&KW_debugger),
    //         "default" => Some(&KW_default),
    //         "do" => Some(&KW_do),
    //         "else" => Some(&KW_else),
    //         "finally" => Some(&KW_finally),
    //         "for" => Some(&KW_for),
    //         "function" => Some(&KW_function),
    //         "if" => Some(&KW_if),
    //         "return" => Some(&KW_return),
    //         "switch" => Some(&KW_switch),
    //         "throw" => Some(&KW_throw),
    //         "try" => Some(&KW_try),
    //         "var" => Some(&KW_var),
    //         "const" => Some(&KW_const),
    //         "while" => Some(&KW_while),
    //         "with" => Some(&KW_with),
    //         "new" => Some(&KW_new),
    //         "this" => Some(&KW_this),
    //         "super" => Some(&KW_super),
    //         "class" => Some(&KW_class),
    //         "extends" => Some(&KW_extends),
    //         "export" => Some(&KW_export),
    //         "import" => Some(&KW_import),
    //         "null" => Some(&KW_null),
    //         "true" => Some(&KW_true),
    //         "false" => Some(&KW_false),
    //         "in" => Some(&KW_in),
    //         "instanceof" => Some(&KW_instanceof),
    //         "typeof" => Some(&KW_typeof),
    //         "void" => Some(&KW_void),
    //         "delete" => Some(&KW_delete),
    //         _ => unreachable!(),
    //     }
    // }
}

// export const types: { [name: string]: TokenType } = {
//   num: new TokenType("num", { startsExpr }),
//   bigint: new TokenType("bigint", { startsExpr }),
//   decimal: new TokenType("decimal", { startsExpr }),
//   regexp: new TokenType("regexp", { startsExpr }),
//   string: new TokenType("string", { startsExpr }),
//   name: new TokenType("name", { startsExpr }),
//   eof: new TokenType("eof"),

//   // Punctuation token types.
//   bracketL: new TokenType("[", { beforeExpr, startsExpr }),
//   bracketHashL: new TokenType("#[", { beforeExpr, startsExpr }),
//   bracketBarL: new TokenType("[|", { beforeExpr, startsExpr }),
//   bracketR: new TokenType("]"),
//   bracketBarR: new TokenType("|]"),
//   braceL: new TokenType("{", { beforeExpr, startsExpr }),
//   braceBarL: new TokenType("{|", { beforeExpr, startsExpr }),
//   braceHashL: new TokenType("#{", { beforeExpr, startsExpr }),
//   braceR: new TokenType("}"),
//   braceBarR: new TokenType("|}"),
//   parenL: new TokenType("(", { beforeExpr, startsExpr }),
//   parenR: new TokenType(")"),
//   comma: new TokenType(",", { beforeExpr }),
//   semi: new TokenType(";", { beforeExpr }),
//   colon: new TokenType(":", { beforeExpr }),
//   doubleColon: new TokenType("::", { beforeExpr }),
//   dot: new TokenType("."),
//   question: new TokenType("?", { beforeExpr }),
//   questionDot: new TokenType("?."),
//   arrow: new TokenType("=>", { beforeExpr }),
//   template: new TokenType("template"),
//   ellipsis: new TokenType("...", { beforeExpr }),
//   backQuote: new TokenType("`", { startsExpr }),
//   dollarBraceL: new TokenType("${", { beforeExpr, startsExpr }),
//   at: new TokenType("@"),
//   hash: new TokenType("#", { startsExpr }),

//   // Special hashbang token.
//   interpreterDirective: new TokenType("#!..."),

//   // Operators. These carry several kinds of properties to help the
//   // parser use them properly (the presence of these properties is
//   // what categorizes them as operators).
//   //
//   // `binop`, when present, specifies that this operator is a binary
//   // operator, and will refer to its precedence.
//   //
//   // `prefix` and `postfix` mark the operator as a prefix or postfix
//   // unary operator.
//   //
//   // `isAssign` marks all of `=`, `+=`, `-=` etcetera, which act as
//   // binary operators with a very low precedence, that should result
//   // in AssignmentExpression nodes.

//   eq: new TokenType("=", { beforeExpr, isAssign }),
//   assign: new TokenType("_=", { beforeExpr, isAssign }),
//   incDec: new TokenType("++/--", { prefix, postfix, startsExpr }),
//   bang: new TokenType("!", { beforeExpr, prefix, startsExpr }),
//   tilde: new TokenType("~", { beforeExpr, prefix, startsExpr }),
//   pipeline: createBinop("|>", 0),
//   nullishCoalescing: createBinop("??", 1),
//   logicalOR: createBinop("||", 1),
//   logicalAND: createBinop("&&", 2),
//   bitwiseOR: createBinop("|", 3),
//   bitwiseXOR: createBinop("^", 4),
//   bitwiseAND: createBinop("&", 5),
//   equality: createBinop("==/!=/===/!==", 6),
//   relational: createBinop("</>/<=/>=", 7),
//   bitShift: createBinop("<</>>/>>>", 8),
//   plusMin: new TokenType("+/-", { beforeExpr, binop: 9, prefix, startsExpr }),
//   // startsExpr: required by v8intrinsic plugin
//   modulo: new TokenType("%", { beforeExpr, binop: 10, startsExpr }),
//   // unset `beforeExpr` as it can be `function *`
//   star: new TokenType("*", { binop: 10 }),
//   slash: createBinop("/", 10),
//   exponent: new TokenType("**", {
//     beforeExpr,
//     binop: 11,
//     rightAssociative: true,
//   }),

//   // Keywords
//   // Don't forget to update packages/babel-helper-validator-identifier/src/keyword.js
//   // when new keywords are added
//   _break: createKeyword("break"),
//   _case: createKeyword("case", { beforeExpr }),
//   _catch: createKeyword("catch"),
//   _continue: createKeyword("continue"),
//   _debugger: createKeyword("debugger"),
//   _default: createKeyword("default", { beforeExpr }),
//   _do: createKeyword("do", { isLoop, beforeExpr }),
//   _else: createKeyword("else", { beforeExpr }),
//   _finally: createKeyword("finally"),
//   _for: createKeyword("for", { isLoop }),
//   _function: createKeyword("function", { startsExpr }),
//   _if: createKeyword("if"),
//   _return: createKeyword("return", { beforeExpr }),
//   _switch: createKeyword("switch"),
//   _throw: createKeyword("throw", { beforeExpr, prefix, startsExpr }),
//   _try: createKeyword("try"),
//   _var: createKeyword("var"),
//   _const: createKeyword("const"),
//   _while: createKeyword("while", { isLoop }),
//   _with: createKeyword("with"),
//   _new: createKeyword("new", { beforeExpr, startsExpr }),
//   _this: createKeyword("this", { startsExpr }),
//   _super: createKeyword("super", { startsExpr }),
//   _class: createKeyword("class", { startsExpr }),
//   _extends: createKeyword("extends", { beforeExpr }),
//   _export: createKeyword("export"),
//   _import: createKeyword("import", { startsExpr }),
//   _null: createKeyword("null", { startsExpr }),
//   _true: createKeyword("true", { startsExpr }),
//   _false: createKeyword("false", { startsExpr }),
//   _in: createKeyword("in", { beforeExpr, binop: 7 }),
//   _instanceof: createKeyword("instanceof", { beforeExpr, binop: 7 }),
//   _typeof: createKeyword("typeof", { beforeExpr, prefix, startsExpr }),
//   _void: createKeyword("void", { beforeExpr, prefix, startsExpr }),
//   _delete: createKeyword("delete", { beforeExpr, prefix, startsExpr }),
// };
