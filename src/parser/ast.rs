// import type { SourceType } from "./options";
// import type { Token } from "./tokenizer";
// import type { SourceLocation } from "./util/location";
// import type { PlaceholderTypes } from "./plugins/placeholders";
// import type { ParsingError } from "./parser/error";

use super::location::SourceLocation;
use crate::lexer::Token;
use crate::options::SourceType;

#[derive(PartialEq, Clone)]
pub enum NodeKind {
    CommentBlock,
    CommentLine,
    Identifier,
    Decorator,
    SpreadElement,
    DebuggerStatement,
    WithStatement,
    ReturnStatement,
    LabeledStatement,
    BreakStatement,
    ContinueStatement,
    IfStatement,
    SwitchStatement,
    SwitchCase,
    ThrowStatement,
    TryStatement,
    CatchClause,
    ExpressionStatement,
    BlockStatement,
    EmptyStatement,
    ObjectPattern,
    ArrayPattern,
    RestElement,
    AssignmentPattern,
    Directive,
    RegExpLiteral,
    NullLiteral,
    StringLiteral,
    BooleanLiteral,
    NumericLiteral,
    BigIntLiteral,
    DirectiveLiteral,

    WhileStatement,
    DoWhileStatement,
    ForStatement,
    ForInStatement,
    ForOfStatement,

    TypeAnnotation,
    TSTypeAnnotation,
    TypeParameterDeclaration,
    TsTypeParameterDeclaration,
    TypeParameter,
    TSTypeParameter,
    TypeParameterInstantiation,
    TSTypeParameterInstantiation,

    TSParameterProperty,

    TSThisType,

    TSDeclareFunction,

    ClassProperty,

    ClassPrivateProperty,

    ClassBody,

    StaticBlock,

    ClassMethod,

    ClassPrivateMethod,

    ClassDeclaration,

    MetaProperty,

    ClassExpression,

    PrivateName,

    FunctionDeclaration,

    VariableDeclaration,

    VariableDeclarator,

    TSExpressionWithTypeArguments,

    TSInterfaceBody,

    TSQualifiedName,

    TSCallSignatureDeclaration,

    TSConstructSignature,

    TSPropertySignature,

    TSMethodSignature,
    TSIndexSignature,
    TSTypeReference,
    TSTypePredicate,
    TSTypeQuery,
    TSTypeLiteral,
    TSArrayType,
    TSTupleType,
    TSNamedTupleMember,
    TSOptionalType,
    TSRestType,
    ImportAttribute,
    Super,
    Import,
    ThisExpression,
    YieldExpression,
    AwaitExpression,
    ArrayExpression,
    DoExpression,
    TupleExpression,
    ObjectExpression,
    RecordExpression,
    ObjectProperty,
    ObjectMethod,
    FunctionExpression,
    ArrowFunctionExpression,

    UnaryExpression,
    UpdateExpression,
    BinaryExpression,

    AssignmentExpression,
    LogicalExpression,

    MemberExpression,

    OptionalMemberExpression,
    BindExpression,
    ConditionalExpression,
    OptionalCallExpression,
    CallExpression,
    NewExpression,
    SequenceExpression,
    ParenthesizedExpression,
    PipelineBody,
    PipelineBareFunctionBody,
    PipelineBareConstructorBody,
    PipelineBareAwaitedFunctionBody,
    PipelineTopicBody,

    TemplateLiteral,
    TaggedTemplateExpression,
    TemplateElement,

    ModuleExpression,

    ImportDeclaration,
    ImportSpecifier,
    ImportDefaultSpecifier,
    ImportNamespaceSpecifier,

    ExportNamedDeclaration,
    ExportSpecifier,
    ExportDefaultSpecifier,
    ExportDefaultDeclaration,
    ExportAllDeclaration,

    JSXOpeningElement,

    TypeCastExpression,
    TSTypeCastExpression,

    FlowInterfaceType,

    TSDeclareMethod,

    File,
    Program,

    TSImportEqualsDeclaration,
    TSExternalModuleReference,

    TsImportType,

    TSFunctionType,
    TSConstructorType,

    TSUnionType,
    TSIntersectionType,

    TSConditionalType,

    TSInferType,
    TSParenthesizedType,
    TSTypeOperator,
    TSIndexedAccessType,
    TSMappedType,
    TSLiteralType,

    TSExportAssignment,
    TSNamespaceExportDeclaration,

    TSAsExpression,
    TSTypeAssertion,
    TSNonNullExpression,

    TSInterfaceDeclaration,

    TSTypeAliasDeclaration,
    TSEnumDeclaration,
    TsEnumMember,
    TSModuleDeclaration,
    TSModuleBlock,

    InterpreterDirective,
}

/*
NodeBase:
  start: usize,
  end: usize,
  loc: SourceLocation,
  range: (usize, usize),
  // leadingComments?: Vec<Comment>;
  // trailingComments?: Vec<Comment>;
  // innerComments?: Vec<Comment>;

HasDecorators:
  decorators: Option<Vec<Decorator>>,
  start: usize,
  end: usize,
  loc: SourceLocation,
  range: (usize, usize),
  // leadingComments?: Vec<Comment>;
  // trailingComments?: Vec<Comment>;
  // innerComments?: Vec<Comment>;

PatternBase:
  // TODO (babel): All not in spec
  // Flow/TypeScript only:
  typeAnnotation?: ?TypeAnnotationBase,

  decorators: Option<Vec<Decorator>>,
  start: usize,
  end: usize,
  loc: SourceLocation,
  range: (usize, usize),
  // leadingComments?: Vec<Comment>;
  // trailingComments?: Vec<Comment>;
  // innerComments?: Vec<Comment>;

BodilessFunctionOrMethodBase
  // TODO (babel): Remove this. Should not assign "id" to methods.
  // https://github.com/babel/babylon/issues/535
  id: Identifier,

  params: Vec<FunctionParams>,
  body: BlockStatement,
  generator: bool,
  is_async: bool,

  // TODO (babel): All not in spec
  expression: bool,
  type_parameters: Option<TypeParameterDeclarationBase>,
  return_type: Option<TypeAnnotationBase>,

  decorators: Option<Vec<Decorator>>,
  start: usize,
  end: usize,
  loc: SourceLocation,
  range: (usize, usize),
  // leadingComments?: Vec<Comment>;
  // trailingComments?: Vec<Comment>;
  // innerComments?: Vec<Comment>;

BodilessFunctionBase
  id: Identifier,

  params: Vec<FunctionParams>,
  body: BlockStatement,
  generator: bool,
  is_async: bool,

  // TODO (babel): All not in spec
  expression: bool,
  type_parameters: Option<TypeParameterDeclarationBase>,
  return_type: Option<TypeAnnotationBase>,

  decorators: Option<Vec<Decorator>>,
  start: usize,
  end: usize,
  loc: SourceLocation,
  range: (usize, usize),
  // leadingComments?: Vec<Comment>;
  // trailingComments?: Vec<Comment>;
  // innerComments?: Vec<Comment>;

TypeParameterDeclarationBase
  params: Vec<TypeParameterBase>,

  start: usize,
  end: usize,
  loc: SourceLocation,
  range: (usize, usize),
  // leadingComments?: Vec<Comment>;
  // trailingComments?: Vec<Comment>;
  // innerComments?: Vec<Comment>;

TypeParameterBase
  name: String,

  start: usize,
  end: usize,
  loc: SourceLocation,
  range: (usize, usize),
  // leadingComments?: Vec<Comment>;
  // trailingComments?: Vec<Comment>;
  // innerComments?: Vec<Comment>;

TypeParameterInstantiationBase
  params: Vec<Node>,

  start: usize,
  end: usize,
  loc: SourceLocation,
  range: (usize, usize),
  // leadingComments?: Vec<Comment>;
  // trailingComments?: Vec<Comment>;
  // innerComments?: Vec<Comment>;

TsSignatureDeclarationBase
  type_parameters: TsTypeParameterDeclaration,

  // Not using TypeScript's "ParameterDeclaration" here, since it's inconsistent with regular functions.
  parameters: Vec<
    Identifier | RestElement | ObjectPattern | ArrayPattern,
  >,
  type_annotation: TsTypeAnnotation,

  start: usize,
  end: usize,
  loc: SourceLocation,
  range: (usize, usize),
  // leadingComments?: Vec<Comment>;
  // trailingComments?: Vec<Comment>;
  // innerComments?: Vec<Comment>;
*/

#[derive(Clone)]
pub struct Comment {
    pub kind: NodeKind,
    pub value: String,
    pub start: usize,
    pub end: usize,
    pub loc: SourceLocation,
}

pub struct NodeBase {
    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum IdentifierValueKind {
    Identifier(Identifier),
    StringLiteral(StringLiteral),
}

enum Expression {}

enum Statement {}

// // Using a union type for `Node` makes type-checking too slow.
// // Instead, add an index signature to allow a Node to be treated as anything.
// export type Node = NodeBase & { [key: string]: any };
enum Pattern {
    Identifier(Box<Identifier>),
    ObjectPattern(Box<ObjectPattern>),
    ArrayPattern(Box<ArrayPattern>),
    RestElement(Box<RestElement>),
    AssignmentPattern(Box<AssignmentPattern>),
}
//| Placeholder<"Pattern">;
enum Declaration {
    VariableDeclaration(VariableDeclaration),
    ClassDeclaration(ClassDeclaration),
    FunctionDeclaration(FunctionDeclaration),
    TsInterfaceDeclaration(TsInterfaceDeclaration),
    TsTypeAliasDeclaration(TsTypeAliasDeclaration),
    TsEnumDeclaration(TsEnumDeclaration),
    TsModuleDeclaration(TsModuleDeclaration),
}
// | Placeholder<"Declaration">;

// // TODO (babel): Not in spec
// export type HasDecorators = NodeBase & {
//   decorators?: Vec<Decorator>,
// };

struct InterpreterDirective {
    kind: NodeKind,
    value: String,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

pub struct Identifier {
    kind: NodeKind,
    name: String,

    // TypeScript only. Used in case of an optional parameter.
    optional: bool,
    decorators: Option<Vec<Decorator>>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;

    //   // TODO (babel): All not in spec
    //   // Flow/TypeScript only:
    //   typeAnnotation?: ?TypeAnnotationBase,
}
// | Placeholder<"Identifier">;

struct PrivateName {
    kind: NodeKind,
    id: Identifier,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

// Literals

enum Literal {
    RegExpLiteral(RegExpLiteral),
    NullLiteral(NullLiteral),
    StringLiteral(StringLiteral),
    BooleanLiteral(BooleanLiteral),
    NumericLiteral(NumericLiteral),
    BigIntLiteral(BigIntLiteral),
}

struct RegExpLiteral {
    kind: NodeKind,
    pattern: String,
    // flags: RegExp$flags,
    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct NullLiteral {
    kind: NodeKind,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct StringLiteral {
    kind: NodeKind,
    value: String,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct BooleanLiteral {
    kind: NodeKind,
    value: bool,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct NumericLiteral {
    kind: NodeKind,
    value: usize,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct BigIntLiteral {
    kind: NodeKind,
    value: usize,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum ParserOutputTokenType<'a> {
    Token(Token<'a>),
    Comment(Comment),
}

struct ParserOutput<'a> {
    comments: Vec<Comment>,
    // errors: Vec<ParsingError>,
    tokens: Option<Vec<ParserOutputTokenType<'a>>>,
}
// Programs

// export type BlockStatementLike = Program | BlockStatement;

struct File<'a> {
    kind: NodeKind,
    program: Program,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
    comments: Vec<Comment>,
    // errors: Vec<ParsingError>,
    tokens: Option<Vec<ParserOutputTokenType<'a>>>,
}

enum ProgramBody {
    Statement(Statement),
    ModuleDeclaration(ModuleDeclaration),
}

struct Program {
    kind: NodeKind,
    source_type: SourceType,
    body: Vec<ProgramBody>,
    directives: Vec<Directive>, // TODO (babel): Not in spec
    interpreter: Option<InterpreterDirective>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

// Functions

// export type Function =
//   | NormalFunction
//   | ArrowFunctionExpression
//   | ObjectMethod
//   | ClassMethod;

// export type NormalFunction = FunctionDeclaration | FunctionExpression;

enum FunctionParams {
    Pattern(Pattern),
    TSParameterProperty(TSParameterProperty),
}

struct BodilessFunctionOrMethodBase {
    // TODO (babel): Remove this. Should not assign "id" to methods.
    // https://github.com/babel/babylon/issues/535
    id: Identifier,

    params: Vec<FunctionParams>,
    body: BlockStatement,
    generator: bool,
    is_async: bool,

    // TODO (babel): All not in spec
    expression: bool,
    type_parameters: Option<TypeParameterDeclarationBase>,
    return_type: Option<TypeAnnotationBase>,

    decorators: Option<Vec<Decorator>>,
    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct BodilessFunctionBase {
    id: Identifier,

    params: Vec<FunctionParams>,
    body: BlockStatement,
    generator: bool,
    is_async: bool,

    // TODO (babel): All not in spec
    expression: bool,
    type_parameters: Option<TypeParameterDeclarationBase>,
    return_type: Option<TypeAnnotationBase>,

    decorators: Option<Vec<Decorator>>,
    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct FunctionBase {
    id: Identifier,

    params: Vec<FunctionParams>,
    body: BlockStatement,
    generator: bool,
    is_async: bool,

    // TODO (babel): All not in spec
    expression: bool,
    type_parameters: Option<TypeParameterDeclarationBase>,
    return_type: Option<TypeAnnotationBase>,

    decorators: Option<Vec<Decorator>>,
    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

// // Statements

struct ExpressionStatement {
    kind: NodeKind,
    expression: Expression,
}

struct BlockStatement {
    kind: NodeKind,
    body: Vec<Statement>,
    directives: Vec<Directive>,
}
// | Placeholder<"BlockStatement">;

struct EmptyStatement {
    kind: NodeKind,
}

struct DebuggerStatement {
    kind: NodeKind,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct WithStatement {
    kind: NodeKind,
    object: Expression,
    body: Statement,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct ReturnStatement {
    kind: NodeKind,
    argument: Expression,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct LabeledStatement {
    kind: NodeKind,
    label: Identifier,
    body: Statement,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct BreakStatement {
    kind: NodeKind,
    label: Identifier,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct ContinueStatement {
    kind: NodeKind,
    label: Identifier,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

// // Choice

struct IfStatement {
    kind: NodeKind,
    test: Expression,
    consequent: Statement,
    alternate: Statement,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct SwitchStatement {
    kind: NodeKind,
    discriminant: Expression,
    cases: Vec<SwitchCase>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct SwitchCase {
    kind: NodeKind,
    test: Expression,
    consequent: Vec<Statement>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

// // Exceptions

struct ThrowStatement {
    kind: NodeKind,
    argument: Expression,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TryStatement {
    kind: NodeKind,
    block: BlockStatement,
    handler: Option<CatchClause>,
    finalizer: Option<BlockStatement>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct CatchClause {
    kind: NodeKind,
    param: Pattern,
    body: BlockStatement,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

// Loops

struct WhileStatement {
    kind: NodeKind,
    test: Expression,
    body: Statement,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct DoWhileStatement {
    kind: NodeKind,
    body: Statement,
    test: Expression,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum ForLike {
    ForStatement(ForStatement),
    ForInOf(ForInOf),
}

enum ForStatementPart {
    VariableDeclaration(VariableDeclaration),
    Expression(Expression),
}

struct ForStatement {
    kind: NodeKind,
    init: ForStatementPart,
    test: Expression,
    update: Expression,
    body: Statement,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum ForInOf {
    ForInStatement(ForInStatement),
    ForOfStatement(ForOfStatement),
}

struct ForInOfBase {
    kind: NodeKind,
    left: ForStatementPart,
    right: Expression,
    body: Statement,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct ForInStatement {
    kind: NodeKind,
    // TODO (babel): Shouldn't be here, but have to declare it because it's assigned to a ForInOf unconditionally.
    is_await: bool,

    left: ForStatementPart,
    right: Expression,
    body: Statement,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct ForOfStatement {
    kind: NodeKind,
    is_await: bool,

    left: ForStatementPart,
    right: Expression,
    body: Statement,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

// // Declarations

struct OptFunctionDeclaration {
    kind: NodeKind,

    id: Identifier,

    params: Vec<FunctionParams>,
    body: BlockStatement,
    generator: bool,
    is_async: bool,

    // TODO (babel): All not in spec
    expression: bool,
    type_parameters: Option<TypeParameterDeclarationBase>,
    return_type: Option<TypeAnnotationBase>,

    decorators: Option<Vec<Decorator>>,
    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;

    // TypeScript allows declarations to be prefixed by `declare`.
    //TODO (babel): a FunctionDeclaration is never "declare", because it's a TSDeclareFunction instead.
    declare: bool,
}

type FunctionDeclaration = OptFunctionDeclaration;

enum VariableDeclarationKeyword {
    Var,
    Let,
    Const,
}

struct VariableDeclaration {
    kind: NodeKind,
    declarations: Vec<VariableDeclarator>,
    keyword: VariableDeclarationKeyword,

    decorators: Option<Vec<Decorator>>,
    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;

    // TypeScript allows declarations to be prefixed by `declare`.
    //TODO (babel): a FunctionDeclaration is never "declare", because it's a TSDeclareFunction instead.
    declare: bool,
}

struct VariableDeclarator {
    kind: NodeKind,
    id: Pattern,
    init: Expression,

    // TypeScript only:
    definite: bool,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

// // Misc

// export type ArgumentPlaceholder = NodeBase & { type: "ArgumentPlaceholder" };

enum Argument {
    Expression(Expression),
    SpreadElement(SpreadElement),
}

pub struct Decorator {
    kind: NodeKind,
    expression: Expression,
    arguments: Option<Vec<Argument>>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct Directive {
    kind: NodeKind,
    value: DirectiveLiteral,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct DirectiveLiteral {
    kind: NodeKind,
    value: String,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct ImportAttribute {
    kind: NodeKind,
    key: IdentifierValueKind,
    value: StringLiteral,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

// Expressions

struct Super {
    kind: NodeKind,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct Import {
    kind: NodeKind,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct ThisExpression {
    kind: NodeKind,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum ArrowFunctionExpressionBodyKind {
    BlockStatement(BlockStatement),
    Expression(Expression),
}

struct ArrowFunctionExpression {
    kind: NodeKind,
    body: ArrowFunctionExpressionBodyKind,
}

struct YieldExpression {
    kind: NodeKind,
    argument: Expression,
    delegate: bool,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct AwaitExpression {
    kind: NodeKind,
    argument: Expression,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum ArrayElement {
    Expression(Expression),
    SpreadElement(SpreadElement),
}

struct ArrayExpression {
    kind: NodeKind,
    elements: Vec<ArrayElement>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct DoExpression {
    kind: NodeKind,
    body: BlockStatement,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TupleExpression {
    kind: NodeKind,
    elements: Vec<ArrayElement>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum ObjectPropertyTypes {
    ObjectProperty(ObjectProperty),
    ObjectMethod(ObjectMethod),
    SpreadElement(SpreadElement),
}

struct ObjectExpression {
    kind: NodeKind,
    properties: Vec<ObjectPropertyTypes>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct RecordExpression {
    kind: NodeKind,
    properties: Vec<ObjectPropertyTypes>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

// export type ObjectOrClassMember = ClassMethod | ClassProperty | ObjectMember;

// export type ObjectMember = ObjectProperty | ObjectMethod;

enum ObjectMemberKind {
    Get,
    Set,
    Method,
}

struct ObjectMemberBase {
    key: Expression,
    computed: bool,
    value: Expression,
    decorators: Vec<Decorator>,
    kind: Option<ObjectMemberKind>,
    method: bool,                                            // TODO (babel): Not in spec
    type_parameters: Option<TypeParameterInstantiationBase>, // TODO (babel): Not in spec
    variance: Option<FlowVariance>,                          // TODO (babel): Not in spec

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct ObjectProperty {
    kind: NodeKind,
    shorthand: bool,

    key: Expression,
    computed: bool,
    value: Expression,
    decorators: Vec<Decorator>,
    method: bool,                                            // TODO (babel): Not in spec
    type_parameters: Option<TypeParameterInstantiationBase>, // TODO (babel): Not in spec
    variance: Option<FlowVariance>,                          // TODO (babel): Not in spec

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum ObjectMethodKind {
    Get,
    Set,
    Method,
}

struct ObjectMethod {
    kind: NodeKind,
    method_kind: ObjectMethodKind, // Never "constructor"

    id: Identifier,

    params: Vec<FunctionParams>,
    body: BlockStatement,
    generator: bool,
    is_async: bool,

    // TODO (babel): All not in spec
    expression: bool,
    type_parameters: Option<TypeParameterDeclarationBase>,
    return_type: Option<TypeAnnotationBase>,

    decorators: Option<Vec<Decorator>>,
    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
    key: Expression,
    computed: bool,
    value: Expression,
    method: bool,                   // TODO (babel): Not in spec
    variance: Option<FlowVariance>, // TODO (babel): Not in spec
}

struct FunctionExpression {
    // kind?: void, // never set
    kind: NodeKind,

    id: Identifier,

    params: Vec<FunctionParams>,
    body: BlockStatement,
    generator: bool,
    is_async: bool,

    // TODO (babel): All not in spec
    expression: bool,
    type_parameters: Option<TypeParameterDeclarationBase>,
    return_type: Option<TypeAnnotationBase>,

    decorators: Option<Vec<Decorator>>,
    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

// Unary operations

struct UnaryExpression {
    kind: NodeKind,
    operator: UnaryOperator,
    prefix: bool,
    argument: Expression,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum UnaryOperator {
    Subtraction, // … - … | - …
    Addition,    // … + … | + …
    LogicalNot,  // ! …
    BitwiseNot,  // ~ …
    Typeof,      // typeof …
    Void,        // void …
    Delete,      // delete …
    Throw,       // throw …
}

struct UpdateExpression {
    kind: NodeKind,
    operator: UpdateOperator,
    argument: Expression,
    prefix: bool,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum UpdateOperator {
    Increment, // … ++ | ++ …
    Decrement, // … -- | -- …
}

// Binary operations

struct BinaryExpression {
    kind: NodeKind,
    operator: BinaryOperator,
    left: Expression,
    right: Expression,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum BinaryOperator {
    Equality,         // … == …
    Inequality,       // … != …
    StrictEquality,   // … === …
    StrictInequality, // … !== …
    Lesser,           // … < …
    LesserEquals,     // … <= …
    Greater,          // … > …
    GreaterEqual,     // … >= …
    BitShiftLeft,     // … << …
    BitShiftRight,    // … >> …
    UBitShiftRight,   // … >>> …

    Multiplication, // … * …
    Division,       // … / …
    Remainder,      // … % …
    BitwiseOr,      // … | …
    BitwiseXor,     // … ^ …
    BitwiseAnd,     // … & …
    In,             // … in …
    InstanceOf,     // … instanceof …

                    // "+"
                    // "-"
}

enum AssignmentExpressionLeft {
    Pattern(Pattern),
    Expression(Expression),
}

struct AssignmentExpression {
    kind: NodeKind,
    operator: AssignmentOperator,
    left: AssignmentExpressionLeft,
    right: Expression,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum AssignmentOperator {
    Assign,          // … = …
    AddAssign,       // … += …
    SubtractAssign,  // … -= …
    MultiplyAssign,  // … *= …
    DivideAssign,    // … /= …
    RemainderAssign, // … %= …
    BSLAssign,       // … <<= …
    BSRAssign,       // … >>= …
    UBSRAssign,      // … >>>= …
    BitOrAssign,     // … |= …
    BitXorAssign,    // … ^= …
    BitAndAssign,    // … &= …
}

struct LogicalExpression {
    kind: NodeKind,
    operator: LogicalOperator,
    left: Expression,
    right: Expression,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum LogicalOperator {
    LogicalOr,  // … || …
    LogicalAnd, // … && …
}

struct SpreadElement {
    kind: NodeKind,
    argument: Expression,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum MemberExpressionObject {
    Expression(Expression),
    Super(Super),
}

struct MemberExpression {
    kind: NodeKind,
    object: MemberExpressionObject,
    property: Expression,
    computed: bool,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct OptionalMemberExpression {
    kind: NodeKind,
    object: MemberExpressionObject,
    property: Expression,
    computed: bool,
    optional: bool,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum FunctionCallee {
    Expression(Expression),
    Super(Super),
    Import(Import),
}

struct OptionalCallExpression {
    kind: NodeKind,
    optional: bool,

    callee: FunctionCallee,
    arguments: Vec<Argument>, // TODO (babel): $ReadOnlyArray
    type_arguments: TypeParameterInstantiationBase,
    type_parameters: Option<TypeParameterInstantiationBase>, // TODO (babel): Not in spec

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct BindExpression {
    kind: NodeKind,
    object: Vec<Expression>,
    callee: Vec<Expression>,
    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct ConditionalExpression {
    kind: NodeKind,
    test: Expression,
    alternate: Expression,
    consequent: Expression,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct CallExpression {
    kind: NodeKind,

    callee: FunctionCallee,
    arguments: Vec<Argument>, // TODO (babel): $ReadOnlyArray
    typeArguments: TypeParameterInstantiationBase,
    typeParameters: Option<TypeParameterInstantiationBase>, // TODO (babel): Not in spec

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct NewExpression {
    kind: NodeKind,
    optional: bool, // TODO (babel): Not in spec

    callee: FunctionCallee,
    arguments: Vec<Argument>, // TODO (babel): $ReadOnlyArray
    typeArguments: TypeParameterInstantiationBase,
    typeParameters: Option<TypeParameterInstantiationBase>, // TODO (babel): Not in spec

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct SequenceExpression {
    kind: NodeKind,
    expressions: Vec<Expression>,
}

struct ParenthesizedExpression {
    kind: NodeKind,
    expression: Expression,
}

// Pipelines

struct PipelineBody {
    kind: NodeKind,
}

struct PipelineBareFunctionBody {
    kind: NodeKind,
    callee: Expression,
}

struct PipelineBareConstructorBody {
    kind: NodeKind,
    callee: Expression,
}

struct PipelineBareAwaitedFunctionBody {
    kind: NodeKind,
    callee: Expression,
}

struct PipelineTopicBody {
    kind: NodeKind,
    expression: Expression,
}

// Template Literals

struct TemplateLiteral {
    kind: NodeKind,
    quasis: Vec<TemplateElement>,
    expressions: Vec<Expression>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TaggedTemplateExpression {
    kind: NodeKind,
    tag: Expression,
    quasi: TemplateLiteral,
    type_parameters: Option<TypeParameterInstantiationBase>, // TODO (babel): Not in spec

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TemplateElementValue {
    cooked: String,
    raw: String,
}

struct TemplateElement {
    kind: NodeKind,
    tail: bool,
    value: TemplateElementValue,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct ModuleExpression {
    kind: NodeKind,
    body: Program,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

// Patterns

// TypeScript access modifiers
enum Accessibility {
    Public,
    Protected,
    Private,
}

struct AssignmentProperty {
    value: Pattern,

    kind: NodeKind,
    shorthand: bool,

    key: Expression,
    computed: bool,
    decorators: Vec<Decorator>,
    method: bool,                                            // TODO (babel): Not in spec
    type_parameters: Option<TypeParameterInstantiationBase>, // TODO (babel): Not in spec
    variance: Option<FlowVariance>,                          // TODO (babel): Not in spec

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum ObjectPatternProperty {
    AssignmentProperty,
    RestElement,
}

struct ObjectPattern {
    kind: NodeKind,
    properties: Vec<ObjectPatternProperty>,

    // TODO (babel): All not in spec
    // Flow/TypeScript only:
    // typeAnnotation?: ?TypeAnnotationBase,
    decorators: Option<Vec<Decorator>>,
    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct ArrayPattern {
    kind: NodeKind,
    elements: Vec<Option<Pattern>>,

    // TODO (babel): All not in spec
    // Flow/TypeScript only:
    // typeAnnotation?: ?TypeAnnotationBase,
    decorators: Option<Vec<Decorator>>,
    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct RestElement {
    kind: NodeKind,
    argument: Pattern,

    // TODO (babel): All not in spec
    // Flow/TypeScript only:
    // typeAnnotation?: ?TypeAnnotationBase,
    decorators: Option<Vec<Decorator>>,
    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct AssignmentPattern {
    kind: NodeKind,
    left: Pattern,
    right: Expression,

    // TODO (babel): All not in spec
    // Flow/TypeScript only:
    // typeAnnotation?: ?TypeAnnotationBase,
    decorators: Option<Vec<Decorator>>,
    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

// // Classes

// export type Class = ClassDeclaration | ClassExpression;

enum ClassBaseImplements {
    TsExpressionWithTypeArguments(TsExpressionWithTypeArguments),
    FlowClassImplements(FlowClassImplements),
}

struct ClassBase {
    id: Identifier,
    super_class: Expression,
    body: ClassBody,
    decorators: Vec<Decorator>,

    // TODO (babel): All not in spec
    type_parameters: Option<TypeParameterDeclarationBase>,
    super_type_parameters: Option<TypeParameterInstantiationBase>,
    implements: Option<ClassBaseImplements>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum ClassBodyContents {
    ClassMember(ClassMember),
    StaticBlock(StaticBlock),
    TsIndexSignature(TsIndexSignature),
}

struct ClassBody {
    kind: NodeKind,
    body: Vec<ClassBodyContents>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}
// | Placeholder<"ClassBody">;

struct ClassMemberBase {
    is_static: bool,
    computed: bool,
    // TypeScript only:
    accessibility: Option<Accessibility>,
    is_abstract: bool,
    optional: bool,

    decorators: Option<Vec<Decorator>>,
    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct StaticBlock {
    kind: NodeKind,
    body: Vec<Statement>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum ClassMember {
    ClassMethod(ClassMethod),
    ClassPrivateMethod(ClassPrivateMethod),
    ClassProperty(ClassProperty),
    ClassPrivateProperty(ClassPrivateProperty),
}

enum MethodLike {
    ObjectMethod(ObjectMethod),
    FunctionExpression(FunctionExpression),
    ClassMethod(ClassMethod),
    ClassPrivateMethod(ClassPrivateMethod),
    TSDeclareMethod(TSDeclareMethod),
}

enum MethodKind {
    Constructor,
    Method,
    Get,
    Set,
}

struct MethodBase {
    kind: MethodKind,

    id: Identifier,

    params: Vec<FunctionParams>,
    body: BlockStatement,
    generator: bool,
    is_async: bool,

    // TODO (babel): All not in spec
    expression: bool,
    type_parameters: Option<TypeParameterDeclarationBase>,
    return_type: Option<TypeAnnotationBase>,

    decorators: Option<Vec<Decorator>>,
    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct ClassMethodOrDeclareMethodCommon {
    key: Expression,
    kind: MethodKind,
    decorators: Vec<Decorator>,

    is_static: bool,
    computed: bool,
    // TypeScript only:
    accessibility: Option<Accessibility>,
    is_abstract: bool,
    optional: bool,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct ClassMethod {
    kind: NodeKind,
    variance: Option<FlowVariance>, // TODO (babel): Not in spec

    key: Expression,
    decorators: Vec<Decorator>,

    is_static: bool,
    computed: bool,
    // TypeScript only:
    accessibility: Option<Accessibility>,
    is_abstract: bool,
    optional: bool,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
    id: Identifier,

    params: Vec<FunctionParams>,
    body: BlockStatement,
    generator: bool,
    is_async: bool,

    // TODO (babel): All not in spec
    expression: bool,
    type_parameters: Option<TypeParameterDeclarationBase>,
    return_type: Option<TypeAnnotationBase>,
}

struct ClassPrivateMethod {
    kind: NodeKind,
    key: PrivateName,
    computed: bool,

    id: Identifier,

    params: Vec<FunctionParams>,
    body: BlockStatement,
    generator: bool,
    is_async: bool,

    // TODO (babel): All not in spec
    expression: bool,
    type_parameters: Option<TypeParameterDeclarationBase>,
    return_type: Option<TypeAnnotationBase>,

    decorators: Option<Vec<Decorator>>,
    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
    is_static: bool,
    // TypeScript only:
    accessibility: Option<Accessibility>,
    is_abstract: bool,
    optional: bool,
}

struct ClassProperty {
    kind: NodeKind,
    key: Expression,
    value: Expression, // TODO (babel): Not in spec that this is nullable.

    type_annotation: Option<TypeAnnotationBase>, // TODO (babel): Not in spec
    variance: Option<FlowVariance>,              // TODO (babel): Not in spec

    // TypeScript only: (TODO (babel): Not in spec)
    readonly: bool,
    definite: bool,

    // TypeScript allows declarations to be prefixed by `declare`.
    //TODO (babel): a FunctionDeclaration is never "declare", because it's a TSDeclareFunction instead.
    declare: bool,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
    is_static: bool,
    computed: bool,
    // TypeScript only:
    accessibility: Option<Accessibility>,
    is_abstract: bool,
    optional: bool,

    decorators: Option<Vec<Decorator>>,
}

struct ClassPrivateProperty {
    kind: NodeKind,
    key: PrivateName,
    value: Expression, // TODO (babel): Not in spec that this is nullable.
    is_static: bool,
    computed: bool,

    // Flow and Typescript
    type_annotation: Option<TypeAnnotationBase>,

    // TypeScript only
    optional: bool,
    definite: bool,
    readonly: bool,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct OptClassDeclaration {
    kind: NodeKind,
    // TypeScript only
    is_abstract: bool,

    id: Identifier,
    super_class: Expression,
    body: ClassBody,
    decorators: Vec<Decorator>,

    // TODO (babel): All not in spec
    type_parameters: Option<TypeParameterDeclarationBase>,
    super_type_parameters: Option<TypeParameterInstantiationBase>,
    implements: Option<ClassBaseImplements>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
    // TypeScript allows declarations to be prefixed by `declare`.
    //TODO (babel): a FunctionDeclaration is never "declare", because it's a TSDeclareFunction instead.
    declare: bool,
}

type ClassDeclaration = OptClassDeclaration;

struct ClassExpression {
    kind: NodeKind,
    id: Identifier,
    super_class: Expression,
    body: ClassBody,
    decorators: Vec<Decorator>,

    // TODO (babel): All not in spec
    type_parameters: Option<TypeParameterDeclarationBase>,
    super_type_parameters: Option<TypeParameterInstantiationBase>,
    implements: Option<ClassBaseImplements>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct MetaProperty {
    kind: NodeKind,
    meta: Identifier,
    property: Identifier,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

// Modules

enum ModuleDeclaration {
    AnyImport(AnyImport),
    AnyExport(AnyExport),
}

enum AnyImport {
    ImportDeclaration(ImportDeclaration),
    TsImportEqualsDeclaration(TsImportEqualsDeclaration),
}

enum AnyExport {
    ExportNamedDeclaration(ExportNamedDeclaration),
    ExportDefaultDeclaration(ExportDefaultDeclaration),
    ExportAllDeclaration(ExportAllDeclaration),
    TsExportAssignment(TsExportAssignment),
    TsImportEqualsDeclaration(TsImportEqualsDeclaration),
    TsNamespaceExportDeclaration(TsNamespaceExportDeclaration),
}

struct ModuleSpecifier {
    local: Identifier,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

// Imports

enum ImportDeclarationSpecifiers {
    ImportSpecifier(ImportSpecifier),
    ImportDefaultSpecifier(ImportDefaultSpecifier),
    ImportNamespaceSpecifier(ImportNamespaceSpecifier),
}

enum ImportKind {
    Type,
    Typeof,
    Value,
}

struct ImportDeclaration {
    kind: NodeKind,
    specifiers: Vec<ImportDeclarationSpecifiers>,
    source: Literal,

    import_kind: Option<ImportKind>, // TODO (babel): Not in spec

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct ImportSpecifier {
    kind: NodeKind,
    imported: IdentifierValueKind,

    local: Identifier,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct ImportDefaultSpecifier {
    kind: NodeKind,

    local: Identifier,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct ImportNamespaceSpecifier {
    kind: NodeKind,

    local: Identifier,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

// Exports

enum ExportKind {
    Type,
    Value,
}

enum ExportNamedDeclaration {
    ExportSpecifier(ExportSpecifier),
    ExportDefaultSpecifier(ExportDefaultSpecifier),
}

struct ExportNamedDeclarationSpecifier {
    kind: NodeKind,
    declaration: Declaration,
    specifiers: Vec<ExportNamedDeclaration>,
    source: Literal,

    export_kind: Option<ExportKind>, // TODO (babel): Not in spec

    assertions: Option<Vec<ImportAttribute>>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct ExportSpecifier {
    kind: NodeKind,
    exported: IdentifierValueKind,
    local: Identifier,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct ExportDefaultSpecifier {
    kind: NodeKind,
    exported: Identifier,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum ExportDefaultDeclarationKind {
    OptFunctionDeclaration(OptFunctionDeclaration),
    OptTSDeclareFunction(OptTSDeclareFunction),
    OptClassDeclaration(OptClassDeclaration),
    Expression(Expression),
}

struct ExportDefaultDeclaration {
    kind: NodeKind,
    declaration: ExportDefaultDeclarationKind,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct ExportAllDeclaration {
    kind: NodeKind,
    source: Literal,
    export_kind: Option<ExportKind>, // TODO (babel): Not in spec

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

// JSX (TODO (babel): Not in spec)

type JSXIdentifier = NodeBase;
type JSXNamespacedName = NodeBase;
type JSXMemberExpression = NodeBase;
type JSXEmptyExpression = NodeBase;
type JSXSpreadChild = NodeBase;
type JSXExpressionContainer = NodeBase;
type JSXAttribute = NodeBase;

enum JSXOpeningElementName {
    JSXNamespacedName(JSXNamespacedName),
    JSXMemberExpression(JSXMemberExpression),
}

struct JSXOpeningElement {
    kind: NodeKind,
    name: JSXOpeningElementName,
    type_parameters: Option<TypeParameterInstantiationBase>, // TODO (babel): Not in spec
    attributes: Vec<JSXAttribute>,
    self_closing: bool,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

type JSXClosingElement = NodeBase;
type JSXElement = NodeBase;
type JSXOpeningFragment = NodeBase;
type JSXClosingFragment = NodeBase;
type JSXFragment = NodeBase;

// Flow/TypeScript common (TODO (babel): Not in spec)

struct TypeAnnotationBase {
    type_annotation: NodeBase,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TypeAnnotation {
    kind: NodeKind,
    type_annotation: FlowTypeAnnotation,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsTypeAnnotation {
    kind: NodeKind,
    type_annotation: TsType,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TypeParameterDeclarationBase {
    params: Vec<TypeParameterBase>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TypeParameterDeclaration {
    kind: NodeKind,
    params: Vec<TypeParameter>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsTypeParameterDeclaration {
    kind: NodeKind,
    params: Vec<TsTypeParameter>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TypeParameterBase {
    name: String,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TypeParameter {
    kind: NodeKind,
    default: Option<TypeAnnotation>,

    name: String,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsTypeParameter {
    kind: NodeKind,
    constraint: Option<TsType>,
    default: Option<TsType>,

    name: String,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TypeParameterInstantiationBase {
    params: Vec<NodeBase>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TypeParameterInstantiation {
    kind: NodeKind,
    params: Vec<FlowType>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsTypeParameterInstantiation {
    kind: NodeKind,
    params: Vec<TsType>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

// Flow (TODO (babel): Not in spec)

struct TypeCastExpressionBase {
    expression: Expression,
    type_annotation: TypeAnnotationBase,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TypeCastExpression {
    kind: NodeKind,
    expression: Expression,
    type_annotation: TypeAnnotation,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsTypeCastExpression {
    kind: NodeKind,
    expression: Expression,
    type_annotation: TsTypeAnnotation,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

type FlowType = NodeBase;
type FlowPredicate = NodeBase;
type FlowDeclare = NodeBase;
type FlowDeclareClass = NodeBase;
type FlowDeclareExportDeclaration = NodeBase;
type FlowDeclareFunction = NodeBase;
type FlowDeclareVariable = NodeBase;
type FlowDeclareModule = NodeBase;
type FlowDeclareModuleExports = NodeBase;
type FlowDeclareTypeAlias = NodeBase;
type FlowDeclareOpaqueType = NodeBase;
type FlowDeclareInterface = NodeBase;
type FlowInterface = NodeBase;
type FlowInterfaceExtends = NodeBase;
type FlowTypeAlias = NodeBase;
type FlowOpaqueType = NodeBase;
type FlowObjectTypeIndexer = NodeBase;
type FlowObjectTypeInternalSlot = NodeBase;
type FlowFunctionTypeAnnotation = NodeBase;
type FlowObjectTypeProperty = NodeBase;
type FlowObjectTypeSpreadProperty = NodeBase;
type FlowObjectTypeCallProperty = NodeBase;
type FlowObjectTypeAnnotation = NodeBase;
type FlowQualifiedTypeIdentifier = NodeBase;
type FlowGenericTypeAnnotation = NodeBase;
type FlowTypeofTypeAnnotation = NodeBase;
type FlowTupleTypeAnnotation = NodeBase;
type FlowFunctionTypeParam = NodeBase;
type FlowTypeAnnotation = NodeBase;
type FlowVariance = NodeBase;
type FlowClassImplements = NodeBase;

struct FlowInterfaceType {
    kind: NodeKind,
    extends: FlowInterfaceExtends,
    body: FlowObjectTypeAnnotation,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

// ESTree

// export type EstreeProperty = NodeBase & {
//   type: "Property",
//   shorthand: bool,
//   key: Expression,
//   computed: bool,
//   value: Expression,
//   decorators: Vec<Decorator>,
//   kind?: "get" | "set" | "init",

//   variance?: ?FlowVariance,
// };

// export type EstreeMethodDefinition = NodeBase & {
//   type: "MethodDefinition",
//   static: bool,
//   key: Expression,
//   computed: bool,
//   value: Expression,
//   decorators: Vec<Decorator>,
//   kind?: "get" | "set" | "method",

//   variance?: ?FlowVariance,
// };

// export type EstreeImportExpression = NodeBase & {
//   type: "ImportExpression",
//   source: Expression,
// };

// export type EstreePrivateIdentifier = NodeBase & {
//   type: "PrivateIdentifier",
//   name: String,
// };

// export type EstreePropertyDefinition = NodeBase & {
//   type: "PropertyDefinition",
//   static: bool,
//   key: Expression | EstreePrivateIdentifier,
//   computed: bool,
//   value: Expression,
// };

// === === === ===
// TypeScript
// === === === ===

// Note: A type named `TsFoo` is based on TypeScript's `FooNode` type,
// defined in https://github.com/Microsoft/TypeScript/blob/master/src/compiler/types.ts
// Differences:
// * Change `NodeArray<T>` to just `Vec<T>`.
// * Don't give nodes a "modifiers" list; use boolean flags instead,
//   and only allow modifiers that are not considered errors.
// * A property named `type` must be renamed to `typeAnnotation` to avoid conflict with the node's type.
// * Sometimes TypeScript allows to parse something which will be a grammar error later;
//   in @babel/parser these cause exceptions, so the AST format is stricter.

// ================
// Misc
// ================

enum TSParameterPropertyModifiers {
    JustReadonly(bool),
    JustAccessibility(Accessibility),
    AccessibilityAndReadonly(Accessibility, bool),
}

enum TSParameterPropertyParam {
    Identifier(Identifier),
    AssignmentPattern(AssignmentPattern),
}

struct TSParameterProperty {
    // Note: This has decorators instead of its parameter.
    kind: NodeKind,
    // At least one of `accessibility` or `readonly` must be set.
    // accessibility?: ?Accessibility,
    // readonly?: ?bool,
    modifiers: TSParameterPropertyModifiers,
    parameter: TSParameterPropertyParam,

    decorators: Option<Vec<Decorator>>,
    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct OptTSDeclareFunction {
    kind: NodeKind,

    // TypeScript allows declarations to be prefixed by `declare`.
    //TODO (babel): a FunctionDeclaration is never "declare", because it's a TSDeclareFunction instead.
    declare: bool,

    id: Identifier,

    params: Vec<FunctionParams>,
    body: BlockStatement,
    generator: bool,
    is_async: bool,

    // TODO (babel): All not in spec
    expression: bool,
    type_parameters: Option<TypeParameterDeclarationBase>,
    return_type: Option<TypeAnnotationBase>,

    decorators: Option<Vec<Decorator>>,
    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TSDeclareFunction {
    id: Identifier,

    kind: NodeKind,

    // TypeScript allows declarations to be prefixed by `declare`.
    //TODO (babel): a FunctionDeclaration is never "declare", because it's a TSDeclareFunction instead.
    declare: bool,

    params: Vec<FunctionParams>,
    body: BlockStatement,
    generator: bool,
    is_async: bool,

    // TODO (babel): All not in spec
    expression: bool,
    type_parameters: Option<TypeParameterDeclarationBase>,
    return_type: Option<TypeAnnotationBase>,

    decorators: Option<Vec<Decorator>>,
    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TSDeclareMethod {
    kind: NodeKind,
    method_kind: MethodKind,

    key: Expression,
    decorators: Vec<Decorator>,

    is_static: bool,
    computed: bool,
    // TypeScript only:
    accessibility: Option<Accessibility>,
    is_abstract: bool,
    optional: bool,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;

    // TODO (babel): Remove this. Should not assign "id" to methods.
    // https://github.com/babel/babylon/issues/535
    id: Identifier,

    params: Vec<FunctionParams>,
    body: BlockStatement,
    generator: bool,
    is_async: bool,

    // TODO (babel): All not in spec
    expression: bool,
    type_parameters: Option<TypeParameterDeclarationBase>,
    return_type: Option<TypeAnnotationBase>,
}

struct TsQualifiedName {
    kind: NodeKind,
    left: TsEntityName,
    right: Identifier,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum TsEntityName {
    Identifier(Identifier),
    TsQualifiedName(Box<TsQualifiedName>),
}

enum TsSignatureDeclaration {
    TsCallSignatureDeclaration(TsCallSignatureDeclaration),
    TsConstructSignatureDeclaration(TsConstructSignatureDeclaration),
    TsMethodSignature(TsMethodSignature),
    TsFunctionType(TsFunctionType),
    TsConstructorType(TsConstructorType),
}

// ================
// TypeScript type members (for type literal / interface / class)
// ================

enum TsTypeElement {
    TsCallSignatureDeclaration(TsCallSignatureDeclaration),
    TsConstructSignatureDeclaration(TsConstructSignatureDeclaration),
    TsPropertySignature(TsPropertySignature),
    TsMethodSignature(TsMethodSignature),
    TsIndexSignature(TsIndexSignature),
}

enum TsSignatureParameter {
    Identifier(Identifier),
    RestElement(RestElement),
    ObjectPattern(ObjectPattern),
    ArrayPattern(ArrayPattern),
}

struct TsCallSignatureDeclaration {
    kind: NodeKind,

    type_parameters: TsTypeParameterDeclaration,

    // Not using TypeScript's "ParameterDeclaration" here, since it's inconsistent with regular functions.
    parameters: Vec<TsSignatureParameter>,
    type_annotation: TsTypeAnnotation,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsConstructSignatureDeclaration {
    kind: NodeKind,

    type_parameters: TsTypeParameterDeclaration,

    // Not using TypeScript's "ParameterDeclaration" here, since it's inconsistent with regular functions.
    parameters: Vec<TsSignatureParameter>,
    type_annotation: TsTypeAnnotation,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsNamedTypeElementBase {
    // Not using TypeScript's `PropertyName` here since we don't have a `ComputedPropertyName` node type.
    // This is usually an Identifier but may be e.g. `Symbol.iterator` if `computed` is true.
    key: Expression,
    computed: bool,
    optional: bool,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsPropertySignature {
    kind: NodeKind,
    readonly: bool,
    type_annotation: Option<TsTypeAnnotation>,
    initializer: Option<Expression>,

    // Not using TypeScript's `PropertyName` here since we don't have a `ComputedPropertyName` node type.
    // This is usually an Identifier but may be e.g. `Symbol.iterator` if `computed` is true.
    key: Expression,
    computed: bool,
    optional: bool,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsMethodSignature {
    kind: NodeKind,

    // Not using TypeScript's `PropertyName` here since we don't have a `ComputedPropertyName` node type.
    // This is usually an Identifier but may be e.g. `Symbol.iterator` if `computed` is true.
    key: Expression,
    computed: bool,
    optional: bool,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
    type_parameters: TsTypeParameterDeclaration,

    // Not using TypeScript's "ParameterDeclaration" here, since it's inconsistent with regular functions.
    parameters: Vec<TsSignatureParameter>,
    type_annotation: TsTypeAnnotation,
}

// *Not* a ClassMemberBase: Can't have accessibility, can't be abstract, can't be optional.
struct TsIndexSignature {
    readonly: bool,
    kind: NodeKind,
    // Note: parameters.length must be 1.

    // Not using TypeScript's "ParameterDeclaration" here, since it's inconsistent with regular functions.
    parameters: Vec<TsSignatureParameter>,
    type_annotation: TsTypeAnnotation,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

// // ================
// // TypeScript types
// // ================

enum TsType {
    TsKeywordType(Box<TsKeywordType>),
    TsThisType(Box<TsThisType>),
    TsFunctionOrConstructorType(Box<TsFunctionOrConstructorType>),
    TsTypeReference(Box<TsTypeReference>),
    TsTypeQuery(Box<TsTypeQuery>),
    TsTypeLiteral(Box<TsTypeLiteral>),
    TsArrayType(Box<TsArrayType>),
    TsTupleType(Box<TsTupleType>),
    TsOptionalType(Box<TsOptionalType>),
    TsRestType(Box<TsRestType>),
    TsUnionOrIntersectionType(Box<TsUnionOrIntersectionType>),
    TsConditionalType(Box<TsConditionalType>),
    TsInferType(Box<TsInferType>),
    TsParenthesizedType(Box<TsParenthesizedType>),
    TsTypeOperator(Box<TsTypeOperator>),
    TsIndexedAccessType(Box<TsIndexedAccessType>),
    TsMappedType(Box<TsMappedType>),
    TsLiteralType(Box<TsLiteralType>),
    TsImportType(Box<TsImportType>),
    // TODO (babel): This probably shouldn't be included here.
    TsTypePredicate(Box<TsTypePredicate>),
}

// export type TsTypeBase = NodeBase;

enum TsKeywordTypeType {
    TSAnyKeyword,
    TSUnknownKeyword,
    TSNumberKeyword,
    TSObjectKeyword,
    TSBooleanKeyword,
    TSBigIntKeyword,
    TSStringKeyword,
    TSSymbolKeyword,
    TSVoidKeyword,
    TSUndefinedKeyword,
    TSNullKeyword,
    TSNeverKeyword,
    TSIntrinsicKeyword,
}

struct TsKeywordType {
    kind: TsKeywordTypeType,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsThisType {
    kind: NodeKind,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum TsFunctionOrConstructorType {
    TsFunctionType(TsFunctionType),
    TsConstructorType(TsConstructorType),
}

struct TsFunctionType {
    kind: NodeKind,
    type_annotation: TypeAnnotation, // not optional

    type_parameters: TsTypeParameterDeclaration,

    // Not using TypeScript's "ParameterDeclaration" here, since it's inconsistent with regular functions.
    parameters: Vec<TsSignatureParameter>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsConstructorType {
    kind: NodeKind,
    type_annotation: TsTypeAnnotation,
    is_abstract: bool,

    type_parameters: TsTypeParameterDeclaration,

    // Not using TypeScript's "ParameterDeclaration" here, since it's inconsistent with regular functions.
    parameters: Vec<TsSignatureParameter>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsTypeReference {
    kind: NodeKind,
    type_name: TsEntityName,
    type_parameters: Option<TsTypeParameterInstantiation>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum TsTypePredicateParameterName {
    Identifier(Identifier),
    TsThisType(TsThisType),
}

struct TsTypePredicate {
    kind: NodeKind,
    parameter_name: TsTypePredicateParameterName,
    type_annotation: TsTypeAnnotation,
    asserts: bool,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum TsTypeQueryExpressionName {
    TsEntityName(TsEntityName),
    TsImportType(TsImportType),
}

// `typeof` operator
struct TsTypeQuery {
    kind: NodeKind,
    expr_name: TsTypeQueryExpressionName,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsTypeLiteral {
    kind: NodeKind,
    members: Vec<TsTypeElement>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsArrayType {
    kind: NodeKind,
    element_type: TsType,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum TsTupleOrRestElementType {
    TsType(Box<TsType>),
    TsNamedTupleMember(Box<TsNamedTupleMember>),
}

struct TsTupleType {
    kind: NodeKind,
    element_types: Vec<TsTupleOrRestElementType>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsNamedTupleMember {
    kind: NodeKind,
    label: Identifier,
    optional: bool,
    element_type: TsType,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsOptionalType {
    kind: NodeKind,
    type_annotation: TsType,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsRestType {
    kind: NodeKind,
    type_annotation: TsTupleOrRestElementType,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum TsUnionOrIntersectionType {
    TsUnionType(TsUnionType),
    TsIntersectionType(TsIntersectionType),
}

struct TsUnionType {
    kind: NodeKind,

    types: Vec<TsType>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsIntersectionType {
    kind: NodeKind,

    types: Vec<TsType>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsConditionalType {
    kind: NodeKind,
    check_type: TsType,
    extends_type: TsType,
    true_type: TsType,
    false_type: TsType,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsInferType {
    kind: NodeKind,
    type_parameter: TypeParameter,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsParenthesizedType {
    kind: NodeKind,
    type_annotation: TsType,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum TsTypeOperatorKind {
    Keyof,
    Uniquem,
    Readonly,
}

struct TsTypeOperator {
    kind: NodeKind,
    operator: TsTypeOperatorKind,
    type_annotation: TsType,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsIndexedAccessType {
    kind: NodeKind,
    object_type: TsType,
    index_type: TsType,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum TsMappedTypeModifierValue {
    True,
    False,
    Plus,
    Minus,
}

struct TsMappedType {
    kind: NodeKind,
    readonly: Option<TsMappedTypeModifierValue>,
    type_parameter: TsTypeParameter,
    optional: Option<TsMappedTypeModifierValue>,
    type_annotation: TsType,
    name_type: TsType,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum TsLiteralKind {
    NumericLiteral(NumericLiteral),
    StringLiteral(StringLiteral),
    BooleanLiteral(BooleanLiteral),
    TemplateLiteral(TemplateLiteral),
}

struct TsLiteralType {
    kind: NodeKind,
    literal: TsLiteralKind,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsImportType {
    kind: NodeKind,
    argument: StringLiteral,
    qualifier: Option<TsEntityName>,
    type_parameters: Option<TsTypeParameterInstantiation>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

// ================
// TypeScript declarations
// ================

struct TsInterfaceDeclaration {
    kind: NodeKind,
    id: Identifier,
    type_parameters: TsTypeParameterDeclaration,
    // TS uses "heritageClauses", but want this to resemble ClassBase.
    extends: Option<Vec<TsExpressionWithTypeArguments>>,
    body: TSInterfaceBody,

    // TypeScript allows declarations to be prefixed by `declare`.
    //TODO (babel): a FunctionDeclaration is never "declare", because it's a TSDeclareFunction instead.
    declare: bool,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TSInterfaceBody {
    kind: NodeKind,
    body: Vec<TsTypeElement>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsExpressionWithTypeArguments {
    kind: NodeKind,
    expression: TsEntityName,
    type_parameters: Option<TsTypeParameterInstantiation>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsTypeAliasDeclaration {
    kind: NodeKind,
    id: Identifier,
    type_parameters: TsTypeParameterDeclaration,
    type_annotation: TsType,

    // TypeScript allows declarations to be prefixed by `declare`.
    //TODO (babel): a FunctionDeclaration is never "declare", because it's a TSDeclareFunction instead.
    declare: bool,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsEnumDeclaration {
    kind: NodeKind,
    is_const: Option<bool>,
    id: Identifier,
    members: Vec<TsEnumMember>,

    // TypeScript allows declarations to be prefixed by `declare`.
    //TODO (babel): a FunctionDeclaration is never "declare", because it's a TSDeclareFunction instead.
    declare: bool,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsEnumMember {
    kind: NodeKind,
    id: IdentifierValueKind,
    initializer: Option<Expression>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsModuleDeclaration {
    kind: NodeKind,
    global: Option<bool>, // In TypeScript, this is only available through `node.flags`.
    id: IdentifierValueKind,
    body: TsNamespaceBody,

    // TypeScript allows declarations to be prefixed by `declare`.
    //TODO (babel): a FunctionDeclaration is never "declare", because it's a TSDeclareFunction instead.
    declare: bool,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

// `namespace A.B { }` is a namespace named `A` with another TsNamespaceDeclaration as its body.
enum TsNamespaceBody {
    TsModuleBlock(Box<TsModuleBlock>),
    TsNamespaceDeclaration(Box<TsNamespaceDeclaration>),
}

struct TsModuleBlock {
    kind: NodeKind,
    body: Vec<Statement>,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsNamespaceDeclaration {
    id: Identifier,
    body: TsNamespaceBody,

    kind: NodeKind,
    global: Option<bool>, // In TypeScript, this is only available through `node.flags`.

    // TypeScript allows declarations to be prefixed by `declare`.
    //TODO (babel): a FunctionDeclaration is never "declare", because it's a TSDeclareFunction instead.
    declare: bool,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum TsImportEqualsDeclarationImportKind {
    Type,
    Value,
}

struct TsImportEqualsDeclaration {
    kind: NodeKind,
    is_export: bool,
    id: Identifier,
    import_kind: TsImportEqualsDeclarationImportKind,
    module_reference: TsModuleReference,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

enum TsModuleReference {
    TsEntityName(TsEntityName),
    TsExternalModuleReference(TsExternalModuleReference),
}

struct TsExternalModuleReference {
    kind: NodeKind,
    expression: StringLiteral,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

// TypeScript's own parser uses ExportAssignment for both `export default` and `export =`.
// But for @babel/parser, `export default` is an ExportDefaultDeclaration,
// so a TsExportAssignment is always `export =`.
struct TsExportAssignment {
    kind: NodeKind,
    expression: Expression,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsNamespaceExportDeclaration {
    kind: NodeKind,
    id: Identifier,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

// ================
// TypeScript expressions
// ================

struct TsAsExpression {
    kind: NodeKind,

    expression: Expression,
    type_annotation: TsType,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsTypeAssertion {
    kind: NodeKind,

    expression: Expression,
    type_annotation: TsType,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

struct TsNonNullExpression {
    kind: NodeKind,
    expression: Expression,

    start: usize,
    end: usize,
    loc: SourceLocation,
    range: (usize, usize),
    // leadingComments?: Vec<Comment>;
    // trailingComments?: Vec<Comment>;
    // innerComments?: Vec<Comment>;
}

// // ================
// // Babel placeholders %%foo%%
// // ================

// export type Placeholder<N: PlaceholderTypes> = NodeBase & {
//   type: "Placeholder",
//   id: Identifier,
//   expectedNode: N,
// };

// // ================
// // Other
// // ================

// export type ParseSubscriptState = {
//   optionalChainMember: bool,
//   maybeAsyncArrow: bool,
//   stop: bool,
// };

// export type ParseClassMemberState = {|
//   hadConstructor: bool,
//   constructorAllowsSuper: bool,
// |};
