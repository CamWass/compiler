use crate::*;
use global_common::EqIgnoreSpan;

index::newtype_index! {
  /// Identifies an AST node.
  pub struct NodeId {
    derive [EqIgnoreSpan]
    DEBUG_FORMAT = "NodeId({})"
  }
}

#[derive(Debug, Copy, Clone)]
pub enum Node<'ast> {
    // class
    Class(&'ast Class<'ast>),
    ClassProp(&'ast ClassProp<'ast>),
    PrivateProp(&'ast PrivateProp<'ast>),
    ClassMethod(&'ast ClassMethod<'ast>),
    PrivateMethod(&'ast PrivateMethod<'ast>),
    Constructor(&'ast Constructor<'ast>),
    Decorator(&'ast Decorator<'ast>),

    // decl
    FnDecl(&'ast FnDecl<'ast>),
    ClassDecl(&'ast ClassDecl<'ast>),
    VarDecl(&'ast VarDecl<'ast>),
    VarDeclarator(&'ast VarDeclarator<'ast>),

    // Expr
    ThisExpr(&'ast ThisExpr),
    ArrayLit(&'ast ArrayLit<'ast>),
    ObjectLit(&'ast ObjectLit<'ast>),
    SpreadElement(&'ast SpreadElement<'ast>),
    UnaryExpr(&'ast UnaryExpr<'ast>),
    UpdateExpr(&'ast UpdateExpr<'ast>),
    BinExpr(&'ast BinExpr<'ast>),
    FnExpr(&'ast FnExpr<'ast>),
    ClassExpr(&'ast ClassExpr<'ast>),
    AssignExpr(&'ast AssignExpr<'ast>),
    MemberExpr(&'ast MemberExpr<'ast>),
    CondExpr(&'ast CondExpr<'ast>),
    CallExpr(&'ast CallExpr<'ast>),
    NewExpr(&'ast NewExpr<'ast>),
    SeqExpr(&'ast SeqExpr<'ast>),
    ArrowExpr(&'ast ArrowExpr<'ast>),
    YieldExpr(&'ast YieldExpr<'ast>),
    MetaPropExpr(&'ast MetaPropExpr<'ast>),
    AwaitExpr(&'ast AwaitExpr<'ast>),
    Tpl(&'ast Tpl<'ast>),
    TaggedTpl(&'ast TaggedTpl<'ast>),
    TplElement(&'ast TplElement<'ast>),
    ParenExpr(&'ast ParenExpr<'ast>),
    Super(&'ast Super),
    ExprOrSpread(&'ast ExprOrSpread<'ast>),
    OptChainExpr(&'ast OptChainExpr<'ast>),

    // function
    Function(&'ast Function<'ast>),
    Param(&'ast Param<'ast>),

    //ident
    BindingIdent(&'ast BindingIdent<'ast>),
    Ident(&'ast Ident),
    PrivateName(&'ast PrivateName<'ast>),

    //jsx
    JSXMemberExpr(&'ast JSXMemberExpr<'ast>),
    JSXNamespacedName(&'ast JSXNamespacedName<'ast>),
    JSXEmptyExpr(&'ast JSXEmptyExpr),
    JSXExprContainer(&'ast JSXExprContainer<'ast>),
    JSXSpreadChild(&'ast JSXSpreadChild<'ast>),
    JSXOpeningElement(&'ast JSXOpeningElement<'ast>),
    JSXClosingElement(&'ast JSXClosingElement<'ast>),
    JSXAttr(&'ast JSXAttr<'ast>),
    JSXText(&'ast JSXText),
    JSXElement(&'ast JSXElement<'ast>),
    JSXFragment(&'ast JSXFragment<'ast>),
    JSXOpeningFragment(&'ast JSXOpeningFragment),
    JSXClosingFragment(&'ast JSXClosingFragment),

    //lib
    Invalid(&'ast Invalid),

    // Lit
    Str(&'ast Str),
    Bool(&'ast Bool),
    Null(&'ast Null),
    Number(&'ast Number),
    BigInt(&'ast BigInt),
    Regex(&'ast Regex),

    // module_decl
    ExportDefaultExpr(&'ast ExportDefaultExpr<'ast>),
    ExportDecl(&'ast ExportDecl<'ast>),
    ImportDecl(&'ast ImportDecl<'ast>),
    ExportAll(&'ast ExportAll<'ast>),
    NamedExport(&'ast NamedExport<'ast>),
    ExportDefaultDecl(&'ast ExportDefaultDecl<'ast>),
    ImportDefaultSpecifier(&'ast ImportDefaultSpecifier<'ast>),
    ImportStarAsSpecifier(&'ast ImportStarAsSpecifier<'ast>),
    ImportNamedSpecifier(&'ast ImportNamedSpecifier<'ast>),
    ExportNamespaceSpecifier(&'ast ExportNamespaceSpecifier<'ast>),
    ExportDefaultSpecifier(&'ast ExportDefaultSpecifier<'ast>),
    ExportNamedSpecifier(&'ast ExportNamedSpecifier<'ast>),

    //module
    Script(&'ast Script<'ast>),
    Module(&'ast Module<'ast>),

    //pat
    ArrayPat(&'ast ArrayPat<'ast>),
    ObjectPat(&'ast ObjectPat<'ast>),
    AssignPat(&'ast AssignPat<'ast>),
    RestPat(&'ast RestPat<'ast>),
    KeyValuePatProp(&'ast KeyValuePatProp<'ast>),
    AssignPatProp(&'ast AssignPatProp<'ast>),

    //prop
    KeyValueProp(&'ast KeyValueProp<'ast>),
    AssignProp(&'ast AssignProp<'ast>),
    GetterProp(&'ast GetterProp<'ast>),
    SetterProp(&'ast SetterProp<'ast>),
    MethodProp(&'ast MethodProp<'ast>),
    ComputedPropName(&'ast ComputedPropName<'ast>),

    //stmt
    BlockStmt(&'ast BlockStmt<'ast>),
    ExprStmt(&'ast ExprStmt<'ast>),
    EmptyStmt(&'ast EmptyStmt),
    DebuggerStmt(&'ast DebuggerStmt),
    WithStmt(&'ast WithStmt<'ast>),
    ReturnStmt(&'ast ReturnStmt<'ast>),
    LabeledStmt(&'ast LabeledStmt<'ast>),
    BreakStmt(&'ast BreakStmt<'ast>),
    ContinueStmt(&'ast ContinueStmt<'ast>),
    IfStmt(&'ast IfStmt<'ast>),
    SwitchStmt(&'ast SwitchStmt<'ast>),
    ThrowStmt(&'ast ThrowStmt<'ast>),
    TryStmt(&'ast TryStmt<'ast>),
    WhileStmt(&'ast WhileStmt<'ast>),
    DoWhileStmt(&'ast DoWhileStmt<'ast>),
    ForStmt(&'ast ForStmt<'ast>),
    ForInStmt(&'ast ForInStmt<'ast>),
    ForOfStmt(&'ast ForOfStmt<'ast>),
    SwitchCase(&'ast SwitchCase<'ast>),
    CatchClause(&'ast CatchClause<'ast>),

    // typescript
    TsTypeAnn(&'ast TsTypeAnn<'ast>),
    TsTypeParamDecl(&'ast TsTypeParamDecl<'ast>),
    TsTypeParam(&'ast TsTypeParam<'ast>),
    TsTypeParamInstantiation(&'ast TsTypeParamInstantiation<'ast>),
    TsParamProp(&'ast TsParamProp<'ast>),
    TsQualifiedName(&'ast TsQualifiedName<'ast>),
    TsCallSignatureDecl(&'ast TsCallSignatureDecl<'ast>),
    TsConstructSignatureDecl(&'ast TsConstructSignatureDecl<'ast>),
    TsPropertySignature(&'ast TsPropertySignature<'ast>),
    TsGetterSignature(&'ast TsGetterSignature<'ast>),
    TsSetterSignature(&'ast TsSetterSignature<'ast>),
    TsMethodSignature(&'ast TsMethodSignature<'ast>),
    TsIndexSignature(&'ast TsIndexSignature<'ast>),
    TsKeywordType(&'ast TsKeywordType),
    TsThisType(&'ast TsThisType),
    TsFnType(&'ast TsFnType<'ast>),
    TsConstructorType(&'ast TsConstructorType<'ast>),
    TsTypeRef(&'ast TsTypeRef<'ast>),
    TsTypePredicate(&'ast TsTypePredicate<'ast>),
    TsTypeQuery(&'ast TsTypeQuery<'ast>),
    TsImportType(&'ast TsImportType<'ast>),
    TsTypeLit(&'ast TsTypeLit<'ast>),
    TsArrayType(&'ast TsArrayType<'ast>),
    TsTupleType(&'ast TsTupleType<'ast>),
    TsTupleElement(&'ast TsTupleElement<'ast>),
    TsOptionalType(&'ast TsOptionalType<'ast>),
    TsRestType(&'ast TsRestType<'ast>),
    TsUnionType(&'ast TsUnionType<'ast>),
    TsIntersectionType(&'ast TsIntersectionType<'ast>),
    TsConditionalType(&'ast TsConditionalType<'ast>),
    TsInferType(&'ast TsInferType<'ast>),
    TsParenthesizedType(&'ast TsParenthesizedType<'ast>),
    TsTypeOperator(&'ast TsTypeOperator<'ast>),
    TsIndexedAccessType(&'ast TsIndexedAccessType<'ast>),
    TsMappedType(&'ast TsMappedType<'ast>),
    TsLitType(&'ast TsLitType<'ast>),
    TsTplLitType(&'ast TsTplLitType<'ast>),
    TsInterfaceDecl(&'ast TsInterfaceDecl<'ast>),
    TsInterfaceBody(&'ast TsInterfaceBody<'ast>),
    TsExprWithTypeArgs(&'ast TsExprWithTypeArgs<'ast>),
    TsTypeAliasDecl(&'ast TsTypeAliasDecl<'ast>),
    TsEnumDecl(&'ast TsEnumDecl<'ast>),
    TsEnumMember(&'ast TsEnumMember<'ast>),
    TsModuleDecl(&'ast TsModuleDecl<'ast>),
    TsModuleBlock(&'ast TsModuleBlock<'ast>),
    TsNamespaceDecl(&'ast TsNamespaceDecl<'ast>),
    TsImportEqualsDecl(&'ast TsImportEqualsDecl<'ast>),
    TsExternalModuleRef(&'ast TsExternalModuleRef<'ast>),
    TsExportAssignment(&'ast TsExportAssignment<'ast>),
    TsNamespaceExportDecl(&'ast TsNamespaceExportDecl<'ast>),
    TsAsExpr(&'ast TsAsExpr<'ast>),
    TsTypeAssertion(&'ast TsTypeAssertion<'ast>),
    TsNonNullExpr(&'ast TsNonNullExpr<'ast>),
    TsConstAssertion(&'ast TsConstAssertion<'ast>),
}
