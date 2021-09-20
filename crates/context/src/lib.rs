// TODO:
#![allow(warnings)]

use ast2::*;
use index::vec::IndexVec;

/// This declares a list of types which can be allocated by `Arena`.
///
/// The `few` modifier will cause allocation to use the shared arena and recording the destructor.
/// This is faster and more memory efficient if there's only a few allocations of the type.
/// Leaving `few` out will cause the type to get its own dedicated `TypedArena` which is
/// faster and more memory efficient if there is lots of allocations.
///
/// Specifying the `decode` modifier will add decode impls for `&T` and `&[T]`,
/// where `T` is the type listed. These impls will appear in the implement_ty_decoder! macro.
#[macro_export]
macro_rules! arena_types {
    ($macro:path, $args:tt, $tcx:lifetime) => (
        $macro!($args, [
            // class
            [] Class: Class<$tcx>,
            [] ClassProp: ClassProp<$tcx>,
            [] PrivateProp: PrivateProp<$tcx>,
            [] ClassMethod: ClassMethod<$tcx>,
            [] PrivateMethod: PrivateMethod<$tcx>,
            [] Constructor: Constructor<$tcx>,
            [] Decorator: Decorator<$tcx>,

            // decl
            [] FnDecl: FnDecl<$tcx>,
            [] ClassDecl: ClassDecl<$tcx>,
            [] VarDecl: VarDecl<$tcx>,
            [] VarDeclarator: VarDeclarator<$tcx>,

            // Expr
            [] ThisExpr: ThisExpr,
            [] ArrayLit: ArrayLit<$tcx>,
            [] ObjectLit: ObjectLit<$tcx>,
            [] SpreadElement: SpreadElement<$tcx>,
            [] UnaryExpr: UnaryExpr<$tcx>,
            [] UpdateExpr: UpdateExpr<$tcx>,
            [] BinExpr: BinExpr<$tcx>,
            [] FnExpr: FnExpr<$tcx>,
            [] ClassExpr: ClassExpr<$tcx>,
            [] AssignExpr: AssignExpr<$tcx>,
            [] MemberExpr: MemberExpr<$tcx>,
            [] CondExpr: CondExpr<$tcx>,
            [] CallExpr: CallExpr<$tcx>,
            [] NewExpr: NewExpr<$tcx>,
            [] SeqExpr: SeqExpr<$tcx>,
            [] ArrowExpr: ArrowExpr<$tcx>,
            [] YieldExpr: YieldExpr<$tcx>,
            [] MetaPropExpr: MetaPropExpr<$tcx>,
            [] AwaitExpr: AwaitExpr<$tcx>,
            [] Tpl: Tpl<$tcx>,
            [] TaggedTpl: TaggedTpl<$tcx>,
            [] TplElement: TplElement<$tcx>,
            [] ParenExpr: ParenExpr<$tcx>,
            [] Super: Super,
            [] ExprOrSpread: ExprOrSpread<$tcx>,
            [] OptChainExpr: OptChainExpr<$tcx>,

            // function
            [] Function: Function<$tcx>,
            [] Param: Param<$tcx>,

            //ident
            [] BindingIdent: BindingIdent<$tcx>,
            [] Ident: Ident,
            [] PrivateName: PrivateName<$tcx>,

            //jsx
            [] JSXMemberExpr: JSXMemberExpr<$tcx>,
            [] JSXNamespacedName: JSXNamespacedName<$tcx>,
            [] JSXEmptyExpr: JSXEmptyExpr,
            [] JSXExprContainer: JSXExprContainer<$tcx>,
            [] JSXSpreadChild: JSXSpreadChild<$tcx>,
            [] JSXOpeningElement: JSXOpeningElement<$tcx>,
            [] JSXClosingElement: JSXClosingElement<$tcx>,
            [] JSXAttr: JSXAttr<$tcx>,
            [] JSXText: JSXText,
            [] JSXElement: JSXElement<$tcx>,
            [] JSXFragment: JSXFragment<$tcx>,
            [] JSXOpeningFragment: JSXOpeningFragment,
            [] JSXClosingFragment: JSXClosingFragment,

            //lib
            [] Invalid: Invalid,

            // Lit
            [] Str: Str,
            [] Bool: Bool,
            [] Null: Null,
            [] Number: Number,
            [] BigInt: BigInt,
            [] Regex: Regex,

            // module_decl
            [] ExportDefaultExpr: ExportDefaultExpr<$tcx>,
            [] ExportDecl: ExportDecl<$tcx>,
            [] ImportDecl: ImportDecl<$tcx>,
            [] ExportAll: ExportAll<$tcx>,
            [] NamedExport: NamedExport<$tcx>,
            [] ExportDefaultDecl: ExportDefaultDecl<$tcx>,
            [] ImportDefaultSpecifier: ImportDefaultSpecifier<$tcx>,
            [] ImportStarAsSpecifier: ImportStarAsSpecifier<$tcx>,
            [] ImportNamedSpecifier: ImportNamedSpecifier<$tcx>,
            [] ExportNamespaceSpecifier: ExportNamespaceSpecifier<$tcx>,
            [] ExportDefaultSpecifier: ExportDefaultSpecifier<$tcx>,
            [] ExportNamedSpecifier: ExportNamedSpecifier<$tcx>,

            //module
            [] Script: Script<$tcx>,
            [] Module: Module<$tcx>,

            //pat
            [] ArrayPat: ArrayPat<$tcx>,
            [] ObjectPat: ObjectPat<$tcx>,
            [] AssignPat: AssignPat<$tcx>,
            [] RestPat: RestPat<$tcx>,
            [] KeyValuePatProp: KeyValuePatProp<$tcx>,
            [] AssignPatProp: AssignPatProp<$tcx>,

            //prop
            [] KeyValueProp: KeyValueProp<$tcx>,
            [] AssignProp: AssignProp<$tcx>,
            [] GetterProp: GetterProp<$tcx>,
            [] SetterProp: SetterProp<$tcx>,
            [] MethodProp: MethodProp<$tcx>,
            [] ComputedPropName: ComputedPropName<$tcx>,

            //stmt
            [] BlockStmt: BlockStmt<$tcx>,
            [] ExprStmt: ExprStmt<$tcx>,
            [] EmptyStmt: EmptyStmt,
            [] DebuggerStmt: DebuggerStmt,
            [] WithStmt: WithStmt<$tcx>,
            [] ReturnStmt: ReturnStmt<$tcx>,
            [] LabeledStmt: LabeledStmt<$tcx>,
            [] BreakStmt: BreakStmt<$tcx>,
            [] ContinueStmt: ContinueStmt<$tcx>,
            [] IfStmt: IfStmt<$tcx>,
            [] SwitchStmt: SwitchStmt<$tcx>,
            [] ThrowStmt: ThrowStmt<$tcx>,
            [] TryStmt: TryStmt<$tcx>,
            [] WhileStmt: WhileStmt<$tcx>,
            [] DoWhileStmt: DoWhileStmt<$tcx>,
            [] ForStmt: ForStmt<$tcx>,
            [] ForInStmt: ForInStmt<$tcx>,
            [] ForOfStmt: ForOfStmt<$tcx>,
            [] SwitchCase: SwitchCase<$tcx>,
            [] CatchClause: CatchClause<$tcx>,

            // typescript
            [] TsTypeAnn: TsTypeAnn<$tcx>,
            [] TsTypeParamDecl: TsTypeParamDecl<$tcx>,
            [] TsTypeParam: TsTypeParam<$tcx>,
            [] TsTypeParamInstantiation: TsTypeParamInstantiation<$tcx>,
            [] TsParamProp: TsParamProp<$tcx>,
            [] TsQualifiedName: TsQualifiedName<$tcx>,
            [] TsCallSignatureDecl: TsCallSignatureDecl<$tcx>,
            [] TsConstructSignatureDecl: TsConstructSignatureDecl<$tcx>,
            [] TsPropertySignature: TsPropertySignature<$tcx>,
            [] TsGetterSignature: TsGetterSignature<$tcx>,
            [] TsSetterSignature: TsSetterSignature<$tcx>,
            [] TsMethodSignature: TsMethodSignature<$tcx>,
            [] TsIndexSignature: TsIndexSignature<$tcx>,
            [] TsKeywordType: TsKeywordType,
            [] TsThisType: TsThisType,
            [] TsFnType: TsFnType<$tcx>,
            [] TsConstructorType: TsConstructorType<$tcx>,
            [] TsTypeRef: TsTypeRef<$tcx>,
            [] TsTypePredicate: TsTypePredicate<$tcx>,
            [] TsTypeQuery: TsTypeQuery<$tcx>,
            [] TsImportType: TsImportType<$tcx>,
            [] TsTypeLit: TsTypeLit<$tcx>,
            [] TsArrayType: TsArrayType<$tcx>,
            [] TsTupleType: TsTupleType<$tcx>,
            [] TsTupleElement: TsTupleElement<$tcx>,
            [] TsOptionalType: TsOptionalType<$tcx>,
            [] TsRestType: TsRestType<$tcx>,
            [] TsUnionType: TsUnionType<$tcx>,
            [] TsIntersectionType: TsIntersectionType<$tcx>,
            [] TsConditionalType: TsConditionalType<$tcx>,
            [] TsInferType: TsInferType<$tcx>,
            [] TsParenthesizedType: TsParenthesizedType<$tcx>,
            [] TsTypeOperator: TsTypeOperator<$tcx>,
            [] TsIndexedAccessType: TsIndexedAccessType<$tcx>,
            [] TsMappedType: TsMappedType<$tcx>,
            [] TsLitType: TsLitType<$tcx>,
            [] TsTplLitType: TsTplLitType<$tcx>,
            [] TsInterfaceDecl: TsInterfaceDecl<$tcx>,
            [] TsInterfaceBody: TsInterfaceBody<$tcx>,
            [] TsExprWithTypeArgs: TsExprWithTypeArgs<$tcx>,
            [] TsTypeAliasDecl: TsTypeAliasDecl<$tcx>,
            [] TsEnumDecl: TsEnumDecl<$tcx>,
            [] TsEnumMember: TsEnumMember<$tcx>,
            [] TsModuleDecl: TsModuleDecl<$tcx>,
            [] TsModuleBlock: TsModuleBlock<$tcx>,
            [] TsNamespaceDecl: TsNamespaceDecl<$tcx>,
            [] TsImportEqualsDecl: TsImportEqualsDecl<$tcx>,
            [] TsExternalModuleRef: TsExternalModuleRef<$tcx>,
            [] TsExportAssignment: TsExportAssignment<$tcx>,
            [] TsNamespaceExportDecl: TsNamespaceExportDecl<$tcx>,
            [] TsAsExpr: TsAsExpr<$tcx>,
            [] TsTypeAssertion: TsTypeAssertion<$tcx>,
            [] TsNonNullExpr: TsNonNullExpr<$tcx>,
            [] TsConstAssertion: TsConstAssertion<$tcx>,
        ], $tcx);
    )
}

arena_types!(arena::declare_arena, [], 'ast);

// pub struct CompileContext<'ctx> {
//   arena: Arena<'ctx>,
//   ast_node_map: Vec<NodeId>
// }

pub struct AstContext<'ast> {
    pub arena: Arena<'ast>,
    pub map: IndexVec<NodeId, Node<'ast>>,
}

impl<'ast> AstContext<'ast> {
    fn new() -> Self {
        Self {
            arena: Arena::default(),
            map: IndexVec::new(),
        }
    }

    // fn next_id(&self) -> NodeId {
    //     self.map.next_index()
    // }

    // fn alloc(&mut self, node: Node<'ast>) -> &'ast Node<'ast>{
    //     self.arena.alloc(node)
    // }
}