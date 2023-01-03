use crate::{our_types::*, types::*};

use std::rc::{Rc, Weak};

#[derive(Debug)]
pub struct BoundNode {
    pub node: Node,
    parent: Weak<BoundNode>,
}

impl BoundNode {
    pub fn parent(&self) -> Option<Rc<BoundNode>> {
        self.parent.upgrade()
    }

    pub fn parent_node(&self) -> Option<Node> {
        self.parent().map(|p| p.node.clone())
    }

    pub fn new(node: Node, parent: Option<&Rc<BoundNode>>) -> Self {
        let parent = match parent {
            Some(p) => Rc::downgrade(p),
            None => Weak::new(),
        };
        Self { node: node, parent }
    }
}

impl HasNodeId for BoundNode {
    fn node_id(&self) -> NodeId {
        self.node.node_id()
    }
}

impl IsNode for BoundNode {
    fn kind(&self) -> SyntaxKind {
        self.node.kind()
    }

    fn name(&self) -> Option<Node> {
        self.node.name()
    }

    fn isPropertyName(&self) -> bool {
        self.node.isPropertyName()
    }

    fn modifiers(&self) -> Option<&NodeArray<Modifier>> {
        self.node.modifiers()
    }
}

pub trait Bind {
    type Bound;
    fn bind(&self, parent: &Rc<BoundNode>) -> Self::Bound {
        self.bind_to_opt_parent(Some(parent))
    }

    fn bind_to_opt_parent(&self, parent: Option<&Rc<BoundNode>>) -> Self::Bound;
}

impl<T> Bind for T
where
    T: Into<Node> + Clone,
{
    type Bound = Rc<BoundNode>;
    fn bind_to_opt_parent(&self, parent: Option<&Rc<BoundNode>>) -> Self::Bound {
        Rc::new(BoundNode::new(self.clone().into(), parent))
    }
}

impl<T> Bind for Option<T>
where
    T: Bind,
{
    type Bound = Option<T::Bound>;
    fn bind_to_opt_parent(&self, parent: Option<&Rc<BoundNode>>) -> Self::Bound {
        self.as_ref().map(|v| v.bind_to_opt_parent(parent))
    }
}

pub trait AsBound {
    type Bound;
    fn as_bound(&self, parent: Option<&Rc<BoundNode>>) -> Self::Bound;
}

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

            fn modifiers(&self) -> Option<&NodeArray<Modifier>>  {
                match self {
                    $(Self::$field(n) => n.modifiers(),)*
                }
            }
        }

        $(
            impl From<Rc<$field>> for Node {
                fn from(inner: Rc<$field>) -> Node {
                    Self::$field(inner)
                }
            }

            impl AsBound for Rc<$field> {
                type Bound = bound::$field;
                fn as_bound(&self, parent: Option<&Rc<BoundNode>>) -> Self::Bound {
                    bound::$field::new(self, parent)
                }
            }
        )*

        pub mod bound {
            use crate::types;
            use crate::our_types;
            use std::rc::{Rc, Weak};
            use super::{BoundNode,Node};

            $(
                #[derive(Debug, Clone)]
                pub struct $field {
                    pub node: Rc<types::$field>,
                    parent: Weak<BoundNode>,
                }

                impl $field {
                    pub fn parent(&self) -> Option<Rc<BoundNode>> {
                        self.parent.upgrade()
                    }

                    pub fn parent_node(&self) -> Option<Node> {
                        self.parent().map(|p| p.node.clone())
                    }

                    pub fn new(node: &Rc<types::$field>, parent: Option<&Rc<BoundNode>>) -> Self {
                        let parent = match parent {
                            Some(p) => Rc::downgrade(p),
                            None => Weak::new(),
                        };
                        Self {
                            node: node.clone(),
                            parent
                        }
                    }
                }

                impl our_types::IsNode for $field {
                    fn kind(&self) -> types::SyntaxKind {
                        self.node.kind()
                    }

                    fn name(&self) -> Option<Node> {
                        self.node.name()
                    }

                    fn isPropertyName(&self) -> bool {
                        self.node.isPropertyName()
                    }

                    fn modifiers(&self) -> Option<&types::NodeArray<types::Modifier>>  {
                        self.node.modifiers()
                    }
                }

                impl our_types::HasNodeId for $field {
                    fn node_id(&self) -> our_types::NodeId {
                        self.node.node_id()
                    }
                }

                impl From<$field> for BoundNode {
                    fn from(other: $field)-> BoundNode {
                        BoundNode::new(other.node.into(), other.parent.upgrade().as_ref())
                    }
                }
            )*
        }


    };
}

make!(
    // Unknown,
    EndOfFileToken,
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
    // Punctuation
    // OpenBraceToken,
    // CloseBraceToken,
    // OpenParenToken,
    // CloseParenToken,
    // OpenBracketToken,
    // CloseBracketToken,
    DotToken,
    DotDotDotToken,
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
    ColonToken,
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
    ImportExpression,
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
    AssertsKeyword,
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
    ConditionalTypeNode,
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
    ExportSpecifier,
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
    HeritageClause,
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
    // Top-level nodes
    SourceFile,
    // Bundle,
    // UnparsedSource,
    // InputFiles,
    // JSDoc nodes
    JSDocTypeExpression,
    // JSDocNameReference,
    // JSDocMemberName,  // C#p
    JSDocAllType,     // The * type
    JSDocUnknownType, // The ? type
    JSDocNullableType,
    JSDocNonNullableType,
    JSDocOptionalType,
    JSDocFunctionType,
    JSDocVariadicType,
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
