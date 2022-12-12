use crate::{our_types::*, types::*};

use std::rc::Rc;

macro_rules! make {
    ($($field:ident,)*) => {
        #[derive(Debug, Clone)]
        pub enum Node {
            $($field(Rc<$field>),)*
        }

        impl HasNodeId for Node {
            fn node_id(&self) -> NodeId {
                match self {
                    $(Self::$field(n) => n.node_id(),)*
                }
            }
        }

        impl IsNode for Node {
            fn kind(&self) -> SyntaxKind {
                match self {
                    $(Self::$field(n) => n.kind(),)*
                }
            }

            fn name(&self) -> Option<Node> {
                match self {
                    $(Self::$field(n) => n.name(),)*
                }
            }

            fn isPropertyName(&self) -> bool {
                match self {
                    $(Self::$field(n) => n.isPropertyName(),)*
                }
            }
        }

        $(
            impl From<Rc<$field>> for Node {
                fn from(inner: Rc<$field>) -> Node {
                    Self::$field(inner)
                }
            }
        )*


    };
}

make!(
    // Unknown,
    // EndOfFileToken,
    // SingleLineCommentTrivia,
    // MultiLineCommentTrivia,
    // NewLineTrivia,
    // WhitespaceTrivia,
    // // We detect and preserve #! on the first line
    // ShebangTrivia,
    // // We detect and provide better error recovery when we encounter a git merge marker.  This
    // // allows us to edit files with git-conflict markers in them in a much more pleasant manner.
    // ConflictMarkerTrivia,
    // Literals
    NumericLiteral,
    BigIntLiteral,
    StringLiteral,
    // JsxText,
    // JsxTextAllWhiteSpaces,
    RegularExpressionLiteral,
    NoSubstitutionTemplateLiteral,
    // Pseudo-literals
    TemplateHead,
    TemplateMiddle,
    TemplateTail,
    // // Punctuation
    // OpenBraceToken,
    // CloseBraceToken,
    // OpenParenToken,
    // CloseParenToken,
    // OpenBracketToken,
    // CloseBracketToken,
    // DotToken,
    // DotDotDotToken,
    // SemicolonToken,
    CommaToken,
    QuestionDotToken,
    LessThanToken,
    // LessThanSlashToken,
    GreaterThanToken,
    LessThanEqualsToken,
    GreaterThanEqualsToken,
    EqualsEqualsToken,
    ExclamationEqualsToken,
    EqualsEqualsEqualsToken,
    ExclamationEqualsEqualsToken,
    EqualsGreaterThanToken,
    PlusToken,
    MinusToken,
    AsteriskToken,
    AsteriskAsteriskToken,
    SlashToken,
    PercentToken,
    // PlusPlusToken,
    // MinusMinusToken,
    LessThanLessThanToken,
    GreaterThanGreaterThanToken,
    GreaterThanGreaterThanGreaterThanToken,
    AmpersandToken,
    BarToken,
    CaretToken,
    ExclamationToken,
    // TildeToken,
    AmpersandAmpersandToken,
    BarBarToken,
    QuestionToken,
    // ColonToken,
    // AtToken,
    QuestionQuestionToken,
    // // Only the JSDoc scanner produces BacktickToken. The normal scanner produces NoSubstitutionTemplateLiteral and related kinds.
    // BacktickToken,
    // // Only the JSDoc scanner produces HashToken. The normal scanner produces PrivateIdentifier.
    // HashToken,
    // // Assignments
    EqualsToken,
    PlusEqualsToken,
    MinusEqualsToken,
    AsteriskEqualsToken,
    AsteriskAsteriskEqualsToken,
    SlashEqualsToken,
    PercentEqualsToken,
    LessThanLessThanEqualsToken,
    GreaterThanGreaterThanEqualsToken,
    GreaterThanGreaterThanGreaterThanEqualsToken,
    AmpersandEqualsToken,
    BarEqualsToken,
    BarBarEqualsToken,
    AmpersandAmpersandEqualsToken,
    QuestionQuestionEqualsToken,
    CaretEqualsToken,
    // Identifiers and PrivateIdentifiers
    Identifier,
    PrivateIdentifier,
    // Reserved words
    // BreakKeyword,
    // CaseKeyword,
    // CatchKeyword,
    // ClassKeyword,
    ConstKeyword,
    // ContinueKeyword,
    // DebuggerKeyword,
    DefaultKeyword,
    // DeleteKeyword,
    // DoKeyword,
    // ElseKeyword,
    // EnumKeyword,
    ExportKeyword,
    // ExtendsKeyword,
    FalseLiteral,
    // FinallyKeyword,
    // ForKeyword,
    // FunctionKeyword,
    // IfKeyword,
    // ImportKeyword,
    InKeyword,
    InstanceOfKeyword,
    // NewKeyword,
    NullLiteral,
    // ReturnKeyword,
    SuperExpression,
    // SwitchKeyword,
    ThisExpression,
    // ThrowKeyword,
    TrueLiteral,
    // TryKeyword,
    // TypeOfKeyword,
    // VarKeyword,
    VoidKeyword,
    // WhileKeyword,
    // WithKeyword,
    // // Strict mode reserved words
    // ImplementsKeyword,
    // InterfaceKeyword,
    // LetKeyword,
    // PackageKeyword,
    PrivateKeyword,
    ProtectedKeyword,
    PublicKeyword,
    StaticKeyword,
    // YieldKeyword,
    // Contextual keywords
    AbstractKeyword,
    // AsKeyword,
    // AssertsKeyword,
    // AssertKeyword,
    AnyKeyword,
    AsyncKeyword,
    AwaitKeyword,
    BooleanKeyword,
    // ConstructorKeyword,
    DeclareKeyword,
    // GetKeyword,
    // InferKeyword,
    IntrinsicKeyword,
    // IsKeyword,
    // KeyOfKeyword,
    // ModuleKeyword,
    // NamespaceKeyword,
    NeverKeyword,
    ReadonlyKeyword,
    // RequireKeyword,
    NumberKeyword,
    ObjectKeyword,
    // SetKeyword,
    StringKeyword,
    SymbolKeyword,
    // TypeKeyword,
    UndefinedKeyword,
    // UniqueKeyword,
    UnknownKeyword,
    // FromKeyword,
    // GlobalKeyword,
    BigIntKeyword,
    OverrideKeyword,
    // OfKeyword, // LastKeyword and LastToken and LastContextualKeyword
    // // Parse tree nodes

    // Names
    QualifiedName,
    ComputedPropertyName,
    // Signature elements
    TypeParameterDeclaration,
    ParameterDeclaration,
    Decorator,
    // TypeMember
    PropertySignature,
    PropertyDeclaration,
    MethodSignature,
    MethodDeclaration,
    ClassStaticBlockDeclaration,
    ConstructorDeclaration,
    GetAccessorDeclaration,
    SetAccessorDeclaration,
    CallSignatureDeclaration,
    ConstructSignatureDeclaration,
    IndexSignatureDeclaration,
    // Type
    TypePredicateNode,
    TypeReferenceNode,
    FunctionTypeNode,
    ConstructorTypeNode,
    TypeQueryNode,
    TypeLiteralNode,
    ArrayTypeNode,
    TupleTypeNode,
    OptionalTypeNode,
    RestTypeNode,
    UnionTypeNode,
    IntersectionTypeNode,
    // ConditionalType,
    InferTypeNode,
    ParenthesizedTypeNode,
    ThisTypeNode,
    TypeOperatorNode,
    IndexedAccessTypeNode,
    MappedTypeNode,
    LiteralTypeNode,
    NamedTupleMember,
    TemplateLiteralTypeNode,
    TemplateLiteralTypeSpan,
    ImportTypeNode,
    // Binding patterns
    ObjectBindingPattern,
    ArrayBindingPattern,
    BindingElement,
    // Expression
    ArrayLiteralExpression,
    ObjectLiteralExpression,
    PropertyAccessExpression,
    ElementAccessExpression,
    CallExpression,
    NewExpression,
    TaggedTemplateExpression,
    TypeAssertion,
    ParenthesizedExpression,
    FunctionExpression,
    ArrowFunction,
    DeleteExpression,
    TypeOfExpression,
    VoidExpression,
    AwaitExpression,
    PrefixUnaryExpression,
    PostfixUnaryExpression,
    BinaryExpression,
    ConditionalExpression,
    TemplateExpression,
    YieldExpression,
    SpreadElement,
    ClassExpression,
    OmittedExpression,
    ExpressionWithTypeArguments,
    AsExpression,
    NonNullExpression,
    MetaProperty,
    // SyntheticExpression,
    // Misc
    TemplateSpan,
    SemicolonClassElement,
    // Element
    Block,
    EmptyStatement,
    VariableStatement,
    ExpressionStatement,
    IfStatement,
    DoStatement,
    WhileStatement,
    ForStatement,
    ForInStatement,
    ForOfStatement,
    ContinueStatement,
    BreakStatement,
    ReturnStatement,
    WithStatement,
    SwitchStatement,
    LabeledStatement,
    ThrowStatement,
    TryStatement,
    DebuggerStatement,
    VariableDeclaration,
    VariableDeclarationList,
    FunctionDeclaration,
    ClassDeclaration,
    InterfaceDeclaration,
    TypeAliasDeclaration,
    EnumDeclaration,
    ModuleDeclaration,
    ModuleBlock,
    CaseBlock,
    NamespaceExportDeclaration,
    ImportEqualsDeclaration,
    ImportDeclaration,
    ImportClause,
    NamespaceImport,
    NamedImports,
    ImportSpecifier,
    ExportAssignment,
    ExportDeclaration,
    NamedExports,
    NamespaceExport,
    // ExportSpecifier,
    MissingDeclaration,
    // Module references
    ExternalModuleReference,
    // JSX
    // JsxElement,
    // JsxSelfClosingElement,
    // JsxOpeningElement,
    // JsxClosingElement,
    // JsxFragment,
    // JsxOpeningFragment,
    // JsxClosingFragment,
    // JsxAttribute,
    // JsxAttributes,
    // JsxSpreadAttribute,
    // JsxExpression,
    // Clauses
    CaseClause,
    DefaultClause,
    // HeritageClause,
    CatchClause,
    AssertClause,
    AssertEntry,
    // Property assignments
    PropertyAssignment,
    ShorthandPropertyAssignment,
    SpreadAssignment,
    // Enum
    EnumMember,
    // // Unparsed
    // UnparsedPrologue,
    // UnparsedPrepend,
    // UnparsedText,
    // UnparsedInternalText,
    // UnparsedSyntheticReference,
    // // Top-level nodes
    // SourceFile,
    // Bundle,
    // UnparsedSource,
    // InputFiles,
    // // JSDoc nodes
    // JSDocTypeExpression,
    // JSDocNameReference,
    // JSDocMemberName,  // C#p
    JSDocAllType,     // The * type
    JSDocUnknownType, // The ? type
    JSDocNullableType,
    JSDocNonNullableType,
    // JSDocOptionalType,
    JSDocFunctionType,
    // JSDocVariadicType,
    // JSDocNamepathType, // https://jsdoc.app/about-namepaths.html
    // JSDocComment,
    // JSDocText,
    // JSDocTypeLiteral,
    // JSDocSignature,
    // JSDocLink,
    // JSDocLinkCode,
    // JSDocLinkPlain,
    // JSDocTag,
    // JSDocAugmentsTag,
    // JSDocImplementsTag,
    // JSDocAuthorTag,
    // JSDocDeprecatedTag,
    // JSDocClassTag,
    // JSDocPublicTag,
    // JSDocPrivateTag,
    // JSDocProtectedTag,
    // JSDocReadonlyTag,
    // JSDocOverrideTag,
    // JSDocCallbackTag,
    // JSDocEnumTag,
    // JSDocParameterTag,
    // JSDocReturnTag,
    // JSDocThisTag,
    // JSDocTypeTag,
    // JSDocTemplateTag,
    // JSDocTypedefTag,
    // JSDocSeeTag,
    // JSDocPropertyTag,
    // // Synthesized list
    // SyntaxList,
    // Transformation nodes
    NotEmittedStatement,
    // PartiallyEmittedExpression,
    // CommaListExpression,
    // MergeDeclarationMarker,
    // EndOfDeclarationMarker,
    // SyntheticReferenceExpression,
);
