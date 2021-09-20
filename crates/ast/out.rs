mod node {
    use crate::*;
    use std::fmt;
    use std::hash::Hash;
    use global_common::Spanned;
    pub enum AstNode<'ast> {
        Class(&'ast Class),
        ClassProp(&'ast ClassProp),
        PrivateProp(&'ast PrivateProp),
        ClassMethod(&'ast ClassMethod),
        PrivateMethod(&'ast PrivateMethod),
        Constructor(&'ast Constructor),
        Decorator(&'ast Decorator),
        FnDecl(&'ast FnDecl),
        ClassDecl(&'ast ClassDecl),
        VarDecl(&'ast VarDecl),
        VarDeclarator(&'ast VarDeclarator),
        ThisExpr(&'ast ThisExpr),
        ArrayLit(&'ast ArrayLit),
        ObjectLit(&'ast ObjectLit),
        SpreadElement(&'ast SpreadElement),
        UnaryExpr(&'ast UnaryExpr),
        UpdateExpr(&'ast UpdateExpr),
        BinExpr(&'ast BinExpr),
        FnExpr(&'ast FnExpr),
        ClassExpr(&'ast ClassExpr),
        AssignExpr(&'ast AssignExpr),
        MemberExpr(&'ast MemberExpr),
        CondExpr(&'ast CondExpr),
        CallExpr(&'ast CallExpr),
        NewExpr(&'ast NewExpr),
        SeqExpr(&'ast SeqExpr),
        ArrowExpr(&'ast ArrowExpr),
        YieldExpr(&'ast YieldExpr),
        MetaPropExpr(&'ast MetaPropExpr),
        AwaitExpr(&'ast AwaitExpr),
        Tpl(&'ast Tpl),
        TaggedTpl(&'ast TaggedTpl),
        TplElement(&'ast TplElement),
        ParenExpr(&'ast ParenExpr),
        Super(&'ast Super),
        ExprOrSpread(&'ast ExprOrSpread),
        OptChainExpr(&'ast OptChainExpr),
        Function(&'ast Function),
        Param(&'ast Param),
        BindingIdent(&'ast BindingIdent),
        Ident(&'ast Ident),
        PrivateName(&'ast PrivateName),
        JSXMemberExpr(&'ast JSXMemberExpr),
        JSXNamespacedName(&'ast JSXNamespacedName),
        JSXEmptyExpr(&'ast JSXEmptyExpr),
        JSXExprContainer(&'ast JSXExprContainer),
        JSXSpreadChild(&'ast JSXSpreadChild),
        JSXOpeningElement(&'ast JSXOpeningElement),
        JSXClosingElement(&'ast JSXClosingElement),
        JSXAttr(&'ast JSXAttr),
        JSXText(&'ast JSXText),
        JSXElement(&'ast JSXElement),
        JSXFragment(&'ast JSXFragment),
        JSXOpeningFragment(&'ast JSXOpeningFragment),
        JSXClosingFragment(&'ast JSXClosingFragment),
        Invalid(&'ast Invalid),
        Str(&'ast Str),
        Bool(&'ast Bool),
        Null(&'ast Null),
        Number(&'ast Number),
        BigInt(&'ast BigInt),
        Regex(&'ast Regex),
        ExportDefaultExpr(&'ast ExportDefaultExpr),
        ExportDecl(&'ast ExportDecl),
        ImportDecl(&'ast ImportDecl),
        ExportAll(&'ast ExportAll),
        NamedExport(&'ast NamedExport),
        ExportDefaultDecl(&'ast ExportDefaultDecl),
        ImportDefaultSpecifier(&'ast ImportDefaultSpecifier),
        ImportStarAsSpecifier(&'ast ImportStarAsSpecifier),
        ImportNamedSpecifier(&'ast ImportNamedSpecifier),
        ExportNamespaceSpecifier(&'ast ExportNamespaceSpecifier),
        ExportDefaultSpecifier(&'ast ExportDefaultSpecifier),
        ExportNamedSpecifier(&'ast ExportNamedSpecifier),
        Script(&'ast Script),
        Module(&'ast Module),
        ArrayPat(&'ast ArrayPat),
        ObjectPat(&'ast ObjectPat),
        AssignPat(&'ast AssignPat),
        RestPat(&'ast RestPat),
        KeyValuePatProp(&'ast KeyValuePatProp),
        AssignPatProp(&'ast AssignPatProp),
        KeyValueProp(&'ast KeyValueProp),
        AssignProp(&'ast AssignProp),
        GetterProp(&'ast GetterProp),
        SetterProp(&'ast SetterProp),
        MethodProp(&'ast MethodProp),
        ComputedPropName(&'ast ComputedPropName),
        BlockStmt(&'ast BlockStmt),
        ExprStmt(&'ast ExprStmt),
        EmptyStmt(&'ast EmptyStmt),
        DebuggerStmt(&'ast DebuggerStmt),
        WithStmt(&'ast WithStmt),
        ReturnStmt(&'ast ReturnStmt),
        LabeledStmt(&'ast LabeledStmt),
        BreakStmt(&'ast BreakStmt),
        ContinueStmt(&'ast ContinueStmt),
        IfStmt(&'ast IfStmt),
        SwitchStmt(&'ast SwitchStmt),
        ThrowStmt(&'ast ThrowStmt),
        TryStmt(&'ast TryStmt),
        WhileStmt(&'ast WhileStmt),
        DoWhileStmt(&'ast DoWhileStmt),
        ForStmt(&'ast ForStmt),
        ForInStmt(&'ast ForInStmt),
        ForOfStmt(&'ast ForOfStmt),
        SwitchCase(&'ast SwitchCase),
        CatchClause(&'ast CatchClause),
        TsTypeAnn(&'ast TsTypeAnn),
        TsTypeParamDecl(&'ast TsTypeParamDecl),
        TsTypeParam(&'ast TsTypeParam),
        TsTypeParamInstantiation(&'ast TsTypeParamInstantiation),
        TsParamProp(&'ast TsParamProp),
        TsQualifiedName(&'ast TsQualifiedName),
        TsCallSignatureDecl(&'ast TsCallSignatureDecl),
        TsConstructSignatureDecl(&'ast TsConstructSignatureDecl),
        TsPropertySignature(&'ast TsPropertySignature),
        TsGetterSignature(&'ast TsGetterSignature),
        TsSetterSignature(&'ast TsSetterSignature),
        TsMethodSignature(&'ast TsMethodSignature),
        TsIndexSignature(&'ast TsIndexSignature),
        TsKeywordType(&'ast TsKeywordType),
        TsThisType(&'ast TsThisType),
        TsFnType(&'ast TsFnType),
        TsConstructorType(&'ast TsConstructorType),
        TsTypeRef(&'ast TsTypeRef),
        TsTypePredicate(&'ast TsTypePredicate),
        TsTypeQuery(&'ast TsTypeQuery),
        TsImportType(&'ast TsImportType),
        TsTypeLit(&'ast TsTypeLit),
        TsArrayType(&'ast TsArrayType),
        TsTupleType(&'ast TsTupleType),
        TsTupleElement(&'ast TsTupleElement),
        TsOptionalType(&'ast TsOptionalType),
        TsRestType(&'ast TsRestType),
        TsUnionType(&'ast TsUnionType),
        TsIntersectionType(&'ast TsIntersectionType),
        TsConditionalType(&'ast TsConditionalType),
        TsInferType(&'ast TsInferType),
        TsParenthesizedType(&'ast TsParenthesizedType),
        TsTypeOperator(&'ast TsTypeOperator),
        TsIndexedAccessType(&'ast TsIndexedAccessType),
        TsMappedType(&'ast TsMappedType),
        TsLitType(&'ast TsLitType),
        TsTplLitType(&'ast TsTplLitType),
        TsInterfaceDecl(&'ast TsInterfaceDecl),
        TsInterfaceBody(&'ast TsInterfaceBody),
        TsExprWithTypeArgs(&'ast TsExprWithTypeArgs),
        TsTypeAliasDecl(&'ast TsTypeAliasDecl),
        TsEnumDecl(&'ast TsEnumDecl),
        TsEnumMember(&'ast TsEnumMember),
        TsModuleDecl(&'ast TsModuleDecl),
        TsModuleBlock(&'ast TsModuleBlock),
        TsNamespaceDecl(&'ast TsNamespaceDecl),
        TsImportEqualsDecl(&'ast TsImportEqualsDecl),
        TsExternalModuleRef(&'ast TsExternalModuleRef),
        TsExportAssignment(&'ast TsExportAssignment),
        TsNamespaceExportDecl(&'ast TsNamespaceExportDecl),
        TsAsExpr(&'ast TsAsExpr),
        TsTypeAssertion(&'ast TsTypeAssertion),
        TsNonNullExpr(&'ast TsNonNullExpr),
        TsConstAssertion(&'ast TsConstAssertion),
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl<'ast> ::core::marker::Copy for AstNode<'ast> {}
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl<'ast> ::core::clone::Clone for AstNode<'ast> {
        #[inline]
        fn clone(&self) -> AstNode<'ast> {
            {
                let _: ::core::clone::AssertParamIsClone<&'ast Class>;
                let _: ::core::clone::AssertParamIsClone<&'ast ClassProp>;
                let _: ::core::clone::AssertParamIsClone<&'ast PrivateProp>;
                let _: ::core::clone::AssertParamIsClone<&'ast ClassMethod>;
                let _: ::core::clone::AssertParamIsClone<&'ast PrivateMethod>;
                let _: ::core::clone::AssertParamIsClone<&'ast Constructor>;
                let _: ::core::clone::AssertParamIsClone<&'ast Decorator>;
                let _: ::core::clone::AssertParamIsClone<&'ast FnDecl>;
                let _: ::core::clone::AssertParamIsClone<&'ast ClassDecl>;
                let _: ::core::clone::AssertParamIsClone<&'ast VarDecl>;
                let _: ::core::clone::AssertParamIsClone<&'ast VarDeclarator>;
                let _: ::core::clone::AssertParamIsClone<&'ast ThisExpr>;
                let _: ::core::clone::AssertParamIsClone<&'ast ArrayLit>;
                let _: ::core::clone::AssertParamIsClone<&'ast ObjectLit>;
                let _: ::core::clone::AssertParamIsClone<&'ast SpreadElement>;
                let _: ::core::clone::AssertParamIsClone<&'ast UnaryExpr>;
                let _: ::core::clone::AssertParamIsClone<&'ast UpdateExpr>;
                let _: ::core::clone::AssertParamIsClone<&'ast BinExpr>;
                let _: ::core::clone::AssertParamIsClone<&'ast FnExpr>;
                let _: ::core::clone::AssertParamIsClone<&'ast ClassExpr>;
                let _: ::core::clone::AssertParamIsClone<&'ast AssignExpr>;
                let _: ::core::clone::AssertParamIsClone<&'ast MemberExpr>;
                let _: ::core::clone::AssertParamIsClone<&'ast CondExpr>;
                let _: ::core::clone::AssertParamIsClone<&'ast CallExpr>;
                let _: ::core::clone::AssertParamIsClone<&'ast NewExpr>;
                let _: ::core::clone::AssertParamIsClone<&'ast SeqExpr>;
                let _: ::core::clone::AssertParamIsClone<&'ast ArrowExpr>;
                let _: ::core::clone::AssertParamIsClone<&'ast YieldExpr>;
                let _: ::core::clone::AssertParamIsClone<&'ast MetaPropExpr>;
                let _: ::core::clone::AssertParamIsClone<&'ast AwaitExpr>;
                let _: ::core::clone::AssertParamIsClone<&'ast Tpl>;
                let _: ::core::clone::AssertParamIsClone<&'ast TaggedTpl>;
                let _: ::core::clone::AssertParamIsClone<&'ast TplElement>;
                let _: ::core::clone::AssertParamIsClone<&'ast ParenExpr>;
                let _: ::core::clone::AssertParamIsClone<&'ast Super>;
                let _: ::core::clone::AssertParamIsClone<&'ast ExprOrSpread>;
                let _: ::core::clone::AssertParamIsClone<&'ast OptChainExpr>;
                let _: ::core::clone::AssertParamIsClone<&'ast Function>;
                let _: ::core::clone::AssertParamIsClone<&'ast Param>;
                let _: ::core::clone::AssertParamIsClone<&'ast BindingIdent>;
                let _: ::core::clone::AssertParamIsClone<&'ast Ident>;
                let _: ::core::clone::AssertParamIsClone<&'ast PrivateName>;
                let _: ::core::clone::AssertParamIsClone<&'ast JSXMemberExpr>;
                let _: ::core::clone::AssertParamIsClone<&'ast JSXNamespacedName>;
                let _: ::core::clone::AssertParamIsClone<&'ast JSXEmptyExpr>;
                let _: ::core::clone::AssertParamIsClone<&'ast JSXExprContainer>;
                let _: ::core::clone::AssertParamIsClone<&'ast JSXSpreadChild>;
                let _: ::core::clone::AssertParamIsClone<&'ast JSXOpeningElement>;
                let _: ::core::clone::AssertParamIsClone<&'ast JSXClosingElement>;
                let _: ::core::clone::AssertParamIsClone<&'ast JSXAttr>;
                let _: ::core::clone::AssertParamIsClone<&'ast JSXText>;
                let _: ::core::clone::AssertParamIsClone<&'ast JSXElement>;
                let _: ::core::clone::AssertParamIsClone<&'ast JSXFragment>;
                let _: ::core::clone::AssertParamIsClone<&'ast JSXOpeningFragment>;
                let _: ::core::clone::AssertParamIsClone<&'ast JSXClosingFragment>;
                let _: ::core::clone::AssertParamIsClone<&'ast Invalid>;
                let _: ::core::clone::AssertParamIsClone<&'ast Str>;
                let _: ::core::clone::AssertParamIsClone<&'ast Bool>;
                let _: ::core::clone::AssertParamIsClone<&'ast Null>;
                let _: ::core::clone::AssertParamIsClone<&'ast Number>;
                let _: ::core::clone::AssertParamIsClone<&'ast BigInt>;
                let _: ::core::clone::AssertParamIsClone<&'ast Regex>;
                let _: ::core::clone::AssertParamIsClone<&'ast ExportDefaultExpr>;
                let _: ::core::clone::AssertParamIsClone<&'ast ExportDecl>;
                let _: ::core::clone::AssertParamIsClone<&'ast ImportDecl>;
                let _: ::core::clone::AssertParamIsClone<&'ast ExportAll>;
                let _: ::core::clone::AssertParamIsClone<&'ast NamedExport>;
                let _: ::core::clone::AssertParamIsClone<&'ast ExportDefaultDecl>;
                let _: ::core::clone::AssertParamIsClone<&'ast ImportDefaultSpecifier>;
                let _: ::core::clone::AssertParamIsClone<&'ast ImportStarAsSpecifier>;
                let _: ::core::clone::AssertParamIsClone<&'ast ImportNamedSpecifier>;
                let _: ::core::clone::AssertParamIsClone<&'ast ExportNamespaceSpecifier>;
                let _: ::core::clone::AssertParamIsClone<&'ast ExportDefaultSpecifier>;
                let _: ::core::clone::AssertParamIsClone<&'ast ExportNamedSpecifier>;
                let _: ::core::clone::AssertParamIsClone<&'ast Script>;
                let _: ::core::clone::AssertParamIsClone<&'ast Module>;
                let _: ::core::clone::AssertParamIsClone<&'ast ArrayPat>;
                let _: ::core::clone::AssertParamIsClone<&'ast ObjectPat>;
                let _: ::core::clone::AssertParamIsClone<&'ast AssignPat>;
                let _: ::core::clone::AssertParamIsClone<&'ast RestPat>;
                let _: ::core::clone::AssertParamIsClone<&'ast KeyValuePatProp>;
                let _: ::core::clone::AssertParamIsClone<&'ast AssignPatProp>;
                let _: ::core::clone::AssertParamIsClone<&'ast KeyValueProp>;
                let _: ::core::clone::AssertParamIsClone<&'ast AssignProp>;
                let _: ::core::clone::AssertParamIsClone<&'ast GetterProp>;
                let _: ::core::clone::AssertParamIsClone<&'ast SetterProp>;
                let _: ::core::clone::AssertParamIsClone<&'ast MethodProp>;
                let _: ::core::clone::AssertParamIsClone<&'ast ComputedPropName>;
                let _: ::core::clone::AssertParamIsClone<&'ast BlockStmt>;
                let _: ::core::clone::AssertParamIsClone<&'ast ExprStmt>;
                let _: ::core::clone::AssertParamIsClone<&'ast EmptyStmt>;
                let _: ::core::clone::AssertParamIsClone<&'ast DebuggerStmt>;
                let _: ::core::clone::AssertParamIsClone<&'ast WithStmt>;
                let _: ::core::clone::AssertParamIsClone<&'ast ReturnStmt>;
                let _: ::core::clone::AssertParamIsClone<&'ast LabeledStmt>;
                let _: ::core::clone::AssertParamIsClone<&'ast BreakStmt>;
                let _: ::core::clone::AssertParamIsClone<&'ast ContinueStmt>;
                let _: ::core::clone::AssertParamIsClone<&'ast IfStmt>;
                let _: ::core::clone::AssertParamIsClone<&'ast SwitchStmt>;
                let _: ::core::clone::AssertParamIsClone<&'ast ThrowStmt>;
                let _: ::core::clone::AssertParamIsClone<&'ast TryStmt>;
                let _: ::core::clone::AssertParamIsClone<&'ast WhileStmt>;
                let _: ::core::clone::AssertParamIsClone<&'ast DoWhileStmt>;
                let _: ::core::clone::AssertParamIsClone<&'ast ForStmt>;
                let _: ::core::clone::AssertParamIsClone<&'ast ForInStmt>;
                let _: ::core::clone::AssertParamIsClone<&'ast ForOfStmt>;
                let _: ::core::clone::AssertParamIsClone<&'ast SwitchCase>;
                let _: ::core::clone::AssertParamIsClone<&'ast CatchClause>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsTypeAnn>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsTypeParamDecl>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsTypeParam>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsTypeParamInstantiation>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsParamProp>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsQualifiedName>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsCallSignatureDecl>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsConstructSignatureDecl>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsPropertySignature>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsGetterSignature>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsSetterSignature>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsMethodSignature>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsIndexSignature>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsKeywordType>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsThisType>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsFnType>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsConstructorType>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsTypeRef>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsTypePredicate>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsTypeQuery>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsImportType>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsTypeLit>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsArrayType>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsTupleType>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsTupleElement>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsOptionalType>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsRestType>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsUnionType>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsIntersectionType>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsConditionalType>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsInferType>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsParenthesizedType>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsTypeOperator>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsIndexedAccessType>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsMappedType>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsLitType>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsTplLitType>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsInterfaceDecl>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsInterfaceBody>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsExprWithTypeArgs>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsTypeAliasDecl>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsEnumDecl>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsEnumMember>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsModuleDecl>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsModuleBlock>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsNamespaceDecl>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsImportEqualsDecl>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsExternalModuleRef>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsExportAssignment>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsNamespaceExportDecl>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsAsExpr>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsTypeAssertion>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsNonNullExpr>;
                let _: ::core::clone::AssertParamIsClone<&'ast TsConstAssertion>;
                *self
            }
        }
    }
    impl<'ast> ::core::marker::StructuralPartialEq for AstNode<'ast> {}
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl<'ast> ::core::cmp::PartialEq for AstNode<'ast> {
        #[inline]
        fn eq(&self, other: &AstNode<'ast>) -> bool {
            {
                let __self_vi = ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi = ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) {
                        (&AstNode::Class(ref __self_0), &AstNode::Class(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::ClassProp(ref __self_0), &AstNode::ClassProp(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (
                            &AstNode::PrivateProp(ref __self_0),
                            &AstNode::PrivateProp(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::ClassMethod(ref __self_0),
                            &AstNode::ClassMethod(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::PrivateMethod(ref __self_0),
                            &AstNode::PrivateMethod(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::Constructor(ref __self_0),
                            &AstNode::Constructor(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (&AstNode::Decorator(ref __self_0), &AstNode::Decorator(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::FnDecl(ref __self_0), &AstNode::FnDecl(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::ClassDecl(ref __self_0), &AstNode::ClassDecl(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::VarDecl(ref __self_0), &AstNode::VarDecl(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (
                            &AstNode::VarDeclarator(ref __self_0),
                            &AstNode::VarDeclarator(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (&AstNode::ThisExpr(ref __self_0), &AstNode::ThisExpr(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::ArrayLit(ref __self_0), &AstNode::ArrayLit(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::ObjectLit(ref __self_0), &AstNode::ObjectLit(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (
                            &AstNode::SpreadElement(ref __self_0),
                            &AstNode::SpreadElement(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (&AstNode::UnaryExpr(ref __self_0), &AstNode::UnaryExpr(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (
                            &AstNode::UpdateExpr(ref __self_0),
                            &AstNode::UpdateExpr(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (&AstNode::BinExpr(ref __self_0), &AstNode::BinExpr(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::FnExpr(ref __self_0), &AstNode::FnExpr(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::ClassExpr(ref __self_0), &AstNode::ClassExpr(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (
                            &AstNode::AssignExpr(ref __self_0),
                            &AstNode::AssignExpr(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::MemberExpr(ref __self_0),
                            &AstNode::MemberExpr(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (&AstNode::CondExpr(ref __self_0), &AstNode::CondExpr(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::CallExpr(ref __self_0), &AstNode::CallExpr(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::NewExpr(ref __self_0), &AstNode::NewExpr(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::SeqExpr(ref __self_0), &AstNode::SeqExpr(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::ArrowExpr(ref __self_0), &AstNode::ArrowExpr(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::YieldExpr(ref __self_0), &AstNode::YieldExpr(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (
                            &AstNode::MetaPropExpr(ref __self_0),
                            &AstNode::MetaPropExpr(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (&AstNode::AwaitExpr(ref __self_0), &AstNode::AwaitExpr(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::Tpl(ref __self_0), &AstNode::Tpl(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::TaggedTpl(ref __self_0), &AstNode::TaggedTpl(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (
                            &AstNode::TplElement(ref __self_0),
                            &AstNode::TplElement(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (&AstNode::ParenExpr(ref __self_0), &AstNode::ParenExpr(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::Super(ref __self_0), &AstNode::Super(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (
                            &AstNode::ExprOrSpread(ref __self_0),
                            &AstNode::ExprOrSpread(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::OptChainExpr(ref __self_0),
                            &AstNode::OptChainExpr(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (&AstNode::Function(ref __self_0), &AstNode::Function(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::Param(ref __self_0), &AstNode::Param(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (
                            &AstNode::BindingIdent(ref __self_0),
                            &AstNode::BindingIdent(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (&AstNode::Ident(ref __self_0), &AstNode::Ident(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (
                            &AstNode::PrivateName(ref __self_0),
                            &AstNode::PrivateName(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::JSXMemberExpr(ref __self_0),
                            &AstNode::JSXMemberExpr(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::JSXNamespacedName(ref __self_0),
                            &AstNode::JSXNamespacedName(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::JSXEmptyExpr(ref __self_0),
                            &AstNode::JSXEmptyExpr(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::JSXExprContainer(ref __self_0),
                            &AstNode::JSXExprContainer(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::JSXSpreadChild(ref __self_0),
                            &AstNode::JSXSpreadChild(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::JSXOpeningElement(ref __self_0),
                            &AstNode::JSXOpeningElement(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::JSXClosingElement(ref __self_0),
                            &AstNode::JSXClosingElement(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (&AstNode::JSXAttr(ref __self_0), &AstNode::JSXAttr(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::JSXText(ref __self_0), &AstNode::JSXText(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (
                            &AstNode::JSXElement(ref __self_0),
                            &AstNode::JSXElement(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::JSXFragment(ref __self_0),
                            &AstNode::JSXFragment(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::JSXOpeningFragment(ref __self_0),
                            &AstNode::JSXOpeningFragment(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::JSXClosingFragment(ref __self_0),
                            &AstNode::JSXClosingFragment(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (&AstNode::Invalid(ref __self_0), &AstNode::Invalid(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::Str(ref __self_0), &AstNode::Str(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::Bool(ref __self_0), &AstNode::Bool(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::Null(ref __self_0), &AstNode::Null(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::Number(ref __self_0), &AstNode::Number(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::BigInt(ref __self_0), &AstNode::BigInt(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::Regex(ref __self_0), &AstNode::Regex(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (
                            &AstNode::ExportDefaultExpr(ref __self_0),
                            &AstNode::ExportDefaultExpr(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::ExportDecl(ref __self_0),
                            &AstNode::ExportDecl(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::ImportDecl(ref __self_0),
                            &AstNode::ImportDecl(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (&AstNode::ExportAll(ref __self_0), &AstNode::ExportAll(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (
                            &AstNode::NamedExport(ref __self_0),
                            &AstNode::NamedExport(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::ExportDefaultDecl(ref __self_0),
                            &AstNode::ExportDefaultDecl(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::ImportDefaultSpecifier(ref __self_0),
                            &AstNode::ImportDefaultSpecifier(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::ImportStarAsSpecifier(ref __self_0),
                            &AstNode::ImportStarAsSpecifier(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::ImportNamedSpecifier(ref __self_0),
                            &AstNode::ImportNamedSpecifier(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::ExportNamespaceSpecifier(ref __self_0),
                            &AstNode::ExportNamespaceSpecifier(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::ExportDefaultSpecifier(ref __self_0),
                            &AstNode::ExportDefaultSpecifier(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::ExportNamedSpecifier(ref __self_0),
                            &AstNode::ExportNamedSpecifier(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (&AstNode::Script(ref __self_0), &AstNode::Script(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::Module(ref __self_0), &AstNode::Module(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::ArrayPat(ref __self_0), &AstNode::ArrayPat(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::ObjectPat(ref __self_0), &AstNode::ObjectPat(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::AssignPat(ref __self_0), &AstNode::AssignPat(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::RestPat(ref __self_0), &AstNode::RestPat(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (
                            &AstNode::KeyValuePatProp(ref __self_0),
                            &AstNode::KeyValuePatProp(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::AssignPatProp(ref __self_0),
                            &AstNode::AssignPatProp(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::KeyValueProp(ref __self_0),
                            &AstNode::KeyValueProp(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::AssignProp(ref __self_0),
                            &AstNode::AssignProp(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::GetterProp(ref __self_0),
                            &AstNode::GetterProp(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::SetterProp(ref __self_0),
                            &AstNode::SetterProp(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::MethodProp(ref __self_0),
                            &AstNode::MethodProp(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::ComputedPropName(ref __self_0),
                            &AstNode::ComputedPropName(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (&AstNode::BlockStmt(ref __self_0), &AstNode::BlockStmt(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::ExprStmt(ref __self_0), &AstNode::ExprStmt(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::EmptyStmt(ref __self_0), &AstNode::EmptyStmt(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (
                            &AstNode::DebuggerStmt(ref __self_0),
                            &AstNode::DebuggerStmt(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (&AstNode::WithStmt(ref __self_0), &AstNode::WithStmt(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (
                            &AstNode::ReturnStmt(ref __self_0),
                            &AstNode::ReturnStmt(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::LabeledStmt(ref __self_0),
                            &AstNode::LabeledStmt(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (&AstNode::BreakStmt(ref __self_0), &AstNode::BreakStmt(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (
                            &AstNode::ContinueStmt(ref __self_0),
                            &AstNode::ContinueStmt(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (&AstNode::IfStmt(ref __self_0), &AstNode::IfStmt(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (
                            &AstNode::SwitchStmt(ref __self_0),
                            &AstNode::SwitchStmt(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (&AstNode::ThrowStmt(ref __self_0), &AstNode::ThrowStmt(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::TryStmt(ref __self_0), &AstNode::TryStmt(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::WhileStmt(ref __self_0), &AstNode::WhileStmt(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (
                            &AstNode::DoWhileStmt(ref __self_0),
                            &AstNode::DoWhileStmt(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (&AstNode::ForStmt(ref __self_0), &AstNode::ForStmt(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::ForInStmt(ref __self_0), &AstNode::ForInStmt(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (&AstNode::ForOfStmt(ref __self_0), &AstNode::ForOfStmt(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (
                            &AstNode::SwitchCase(ref __self_0),
                            &AstNode::SwitchCase(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::CatchClause(ref __self_0),
                            &AstNode::CatchClause(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (&AstNode::TsTypeAnn(ref __self_0), &AstNode::TsTypeAnn(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (
                            &AstNode::TsTypeParamDecl(ref __self_0),
                            &AstNode::TsTypeParamDecl(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsTypeParam(ref __self_0),
                            &AstNode::TsTypeParam(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsTypeParamInstantiation(ref __self_0),
                            &AstNode::TsTypeParamInstantiation(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsParamProp(ref __self_0),
                            &AstNode::TsParamProp(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsQualifiedName(ref __self_0),
                            &AstNode::TsQualifiedName(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsCallSignatureDecl(ref __self_0),
                            &AstNode::TsCallSignatureDecl(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsConstructSignatureDecl(ref __self_0),
                            &AstNode::TsConstructSignatureDecl(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsPropertySignature(ref __self_0),
                            &AstNode::TsPropertySignature(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsGetterSignature(ref __self_0),
                            &AstNode::TsGetterSignature(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsSetterSignature(ref __self_0),
                            &AstNode::TsSetterSignature(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsMethodSignature(ref __self_0),
                            &AstNode::TsMethodSignature(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsIndexSignature(ref __self_0),
                            &AstNode::TsIndexSignature(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsKeywordType(ref __self_0),
                            &AstNode::TsKeywordType(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsThisType(ref __self_0),
                            &AstNode::TsThisType(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (&AstNode::TsFnType(ref __self_0), &AstNode::TsFnType(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (
                            &AstNode::TsConstructorType(ref __self_0),
                            &AstNode::TsConstructorType(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (&AstNode::TsTypeRef(ref __self_0), &AstNode::TsTypeRef(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (
                            &AstNode::TsTypePredicate(ref __self_0),
                            &AstNode::TsTypePredicate(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsTypeQuery(ref __self_0),
                            &AstNode::TsTypeQuery(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsImportType(ref __self_0),
                            &AstNode::TsImportType(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (&AstNode::TsTypeLit(ref __self_0), &AstNode::TsTypeLit(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (
                            &AstNode::TsArrayType(ref __self_0),
                            &AstNode::TsArrayType(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsTupleType(ref __self_0),
                            &AstNode::TsTupleType(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsTupleElement(ref __self_0),
                            &AstNode::TsTupleElement(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsOptionalType(ref __self_0),
                            &AstNode::TsOptionalType(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsRestType(ref __self_0),
                            &AstNode::TsRestType(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsUnionType(ref __self_0),
                            &AstNode::TsUnionType(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsIntersectionType(ref __self_0),
                            &AstNode::TsIntersectionType(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsConditionalType(ref __self_0),
                            &AstNode::TsConditionalType(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsInferType(ref __self_0),
                            &AstNode::TsInferType(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsParenthesizedType(ref __self_0),
                            &AstNode::TsParenthesizedType(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsTypeOperator(ref __self_0),
                            &AstNode::TsTypeOperator(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsIndexedAccessType(ref __self_0),
                            &AstNode::TsIndexedAccessType(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsMappedType(ref __self_0),
                            &AstNode::TsMappedType(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (&AstNode::TsLitType(ref __self_0), &AstNode::TsLitType(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (
                            &AstNode::TsTplLitType(ref __self_0),
                            &AstNode::TsTplLitType(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsInterfaceDecl(ref __self_0),
                            &AstNode::TsInterfaceDecl(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsInterfaceBody(ref __self_0),
                            &AstNode::TsInterfaceBody(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsExprWithTypeArgs(ref __self_0),
                            &AstNode::TsExprWithTypeArgs(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsTypeAliasDecl(ref __self_0),
                            &AstNode::TsTypeAliasDecl(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsEnumDecl(ref __self_0),
                            &AstNode::TsEnumDecl(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsEnumMember(ref __self_0),
                            &AstNode::TsEnumMember(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsModuleDecl(ref __self_0),
                            &AstNode::TsModuleDecl(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsModuleBlock(ref __self_0),
                            &AstNode::TsModuleBlock(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsNamespaceDecl(ref __self_0),
                            &AstNode::TsNamespaceDecl(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsImportEqualsDecl(ref __self_0),
                            &AstNode::TsImportEqualsDecl(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsExternalModuleRef(ref __self_0),
                            &AstNode::TsExternalModuleRef(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsExportAssignment(ref __self_0),
                            &AstNode::TsExportAssignment(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsNamespaceExportDecl(ref __self_0),
                            &AstNode::TsNamespaceExportDecl(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (&AstNode::TsAsExpr(ref __self_0), &AstNode::TsAsExpr(ref __arg_1_0)) => {
                            (*__self_0) == (*__arg_1_0)
                        }
                        (
                            &AstNode::TsTypeAssertion(ref __self_0),
                            &AstNode::TsTypeAssertion(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsNonNullExpr(ref __self_0),
                            &AstNode::TsNonNullExpr(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        (
                            &AstNode::TsConstAssertion(ref __self_0),
                            &AstNode::TsConstAssertion(ref __arg_1_0),
                        ) => (*__self_0) == (*__arg_1_0),
                        _ => unsafe { ::core::intrinsics::unreachable() },
                    }
                } else {
                    false
                }
            }
        }
        #[inline]
        fn ne(&self, other: &AstNode<'ast>) -> bool {
            {
                let __self_vi = ::core::intrinsics::discriminant_value(&*self);
                let __arg_1_vi = ::core::intrinsics::discriminant_value(&*other);
                if true && __self_vi == __arg_1_vi {
                    match (&*self, &*other) {
                        (&AstNode::Class(ref __self_0), &AstNode::Class(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::ClassProp(ref __self_0), &AstNode::ClassProp(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (
                            &AstNode::PrivateProp(ref __self_0),
                            &AstNode::PrivateProp(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::ClassMethod(ref __self_0),
                            &AstNode::ClassMethod(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::PrivateMethod(ref __self_0),
                            &AstNode::PrivateMethod(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::Constructor(ref __self_0),
                            &AstNode::Constructor(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (&AstNode::Decorator(ref __self_0), &AstNode::Decorator(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::FnDecl(ref __self_0), &AstNode::FnDecl(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::ClassDecl(ref __self_0), &AstNode::ClassDecl(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::VarDecl(ref __self_0), &AstNode::VarDecl(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (
                            &AstNode::VarDeclarator(ref __self_0),
                            &AstNode::VarDeclarator(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (&AstNode::ThisExpr(ref __self_0), &AstNode::ThisExpr(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::ArrayLit(ref __self_0), &AstNode::ArrayLit(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::ObjectLit(ref __self_0), &AstNode::ObjectLit(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (
                            &AstNode::SpreadElement(ref __self_0),
                            &AstNode::SpreadElement(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (&AstNode::UnaryExpr(ref __self_0), &AstNode::UnaryExpr(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (
                            &AstNode::UpdateExpr(ref __self_0),
                            &AstNode::UpdateExpr(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (&AstNode::BinExpr(ref __self_0), &AstNode::BinExpr(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::FnExpr(ref __self_0), &AstNode::FnExpr(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::ClassExpr(ref __self_0), &AstNode::ClassExpr(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (
                            &AstNode::AssignExpr(ref __self_0),
                            &AstNode::AssignExpr(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::MemberExpr(ref __self_0),
                            &AstNode::MemberExpr(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (&AstNode::CondExpr(ref __self_0), &AstNode::CondExpr(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::CallExpr(ref __self_0), &AstNode::CallExpr(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::NewExpr(ref __self_0), &AstNode::NewExpr(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::SeqExpr(ref __self_0), &AstNode::SeqExpr(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::ArrowExpr(ref __self_0), &AstNode::ArrowExpr(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::YieldExpr(ref __self_0), &AstNode::YieldExpr(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (
                            &AstNode::MetaPropExpr(ref __self_0),
                            &AstNode::MetaPropExpr(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (&AstNode::AwaitExpr(ref __self_0), &AstNode::AwaitExpr(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::Tpl(ref __self_0), &AstNode::Tpl(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::TaggedTpl(ref __self_0), &AstNode::TaggedTpl(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (
                            &AstNode::TplElement(ref __self_0),
                            &AstNode::TplElement(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (&AstNode::ParenExpr(ref __self_0), &AstNode::ParenExpr(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::Super(ref __self_0), &AstNode::Super(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (
                            &AstNode::ExprOrSpread(ref __self_0),
                            &AstNode::ExprOrSpread(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::OptChainExpr(ref __self_0),
                            &AstNode::OptChainExpr(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (&AstNode::Function(ref __self_0), &AstNode::Function(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::Param(ref __self_0), &AstNode::Param(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (
                            &AstNode::BindingIdent(ref __self_0),
                            &AstNode::BindingIdent(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (&AstNode::Ident(ref __self_0), &AstNode::Ident(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (
                            &AstNode::PrivateName(ref __self_0),
                            &AstNode::PrivateName(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::JSXMemberExpr(ref __self_0),
                            &AstNode::JSXMemberExpr(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::JSXNamespacedName(ref __self_0),
                            &AstNode::JSXNamespacedName(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::JSXEmptyExpr(ref __self_0),
                            &AstNode::JSXEmptyExpr(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::JSXExprContainer(ref __self_0),
                            &AstNode::JSXExprContainer(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::JSXSpreadChild(ref __self_0),
                            &AstNode::JSXSpreadChild(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::JSXOpeningElement(ref __self_0),
                            &AstNode::JSXOpeningElement(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::JSXClosingElement(ref __self_0),
                            &AstNode::JSXClosingElement(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (&AstNode::JSXAttr(ref __self_0), &AstNode::JSXAttr(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::JSXText(ref __self_0), &AstNode::JSXText(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (
                            &AstNode::JSXElement(ref __self_0),
                            &AstNode::JSXElement(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::JSXFragment(ref __self_0),
                            &AstNode::JSXFragment(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::JSXOpeningFragment(ref __self_0),
                            &AstNode::JSXOpeningFragment(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::JSXClosingFragment(ref __self_0),
                            &AstNode::JSXClosingFragment(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (&AstNode::Invalid(ref __self_0), &AstNode::Invalid(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::Str(ref __self_0), &AstNode::Str(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::Bool(ref __self_0), &AstNode::Bool(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::Null(ref __self_0), &AstNode::Null(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::Number(ref __self_0), &AstNode::Number(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::BigInt(ref __self_0), &AstNode::BigInt(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::Regex(ref __self_0), &AstNode::Regex(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (
                            &AstNode::ExportDefaultExpr(ref __self_0),
                            &AstNode::ExportDefaultExpr(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::ExportDecl(ref __self_0),
                            &AstNode::ExportDecl(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::ImportDecl(ref __self_0),
                            &AstNode::ImportDecl(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (&AstNode::ExportAll(ref __self_0), &AstNode::ExportAll(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (
                            &AstNode::NamedExport(ref __self_0),
                            &AstNode::NamedExport(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::ExportDefaultDecl(ref __self_0),
                            &AstNode::ExportDefaultDecl(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::ImportDefaultSpecifier(ref __self_0),
                            &AstNode::ImportDefaultSpecifier(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::ImportStarAsSpecifier(ref __self_0),
                            &AstNode::ImportStarAsSpecifier(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::ImportNamedSpecifier(ref __self_0),
                            &AstNode::ImportNamedSpecifier(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::ExportNamespaceSpecifier(ref __self_0),
                            &AstNode::ExportNamespaceSpecifier(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::ExportDefaultSpecifier(ref __self_0),
                            &AstNode::ExportDefaultSpecifier(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::ExportNamedSpecifier(ref __self_0),
                            &AstNode::ExportNamedSpecifier(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (&AstNode::Script(ref __self_0), &AstNode::Script(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::Module(ref __self_0), &AstNode::Module(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::ArrayPat(ref __self_0), &AstNode::ArrayPat(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::ObjectPat(ref __self_0), &AstNode::ObjectPat(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::AssignPat(ref __self_0), &AstNode::AssignPat(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::RestPat(ref __self_0), &AstNode::RestPat(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (
                            &AstNode::KeyValuePatProp(ref __self_0),
                            &AstNode::KeyValuePatProp(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::AssignPatProp(ref __self_0),
                            &AstNode::AssignPatProp(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::KeyValueProp(ref __self_0),
                            &AstNode::KeyValueProp(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::AssignProp(ref __self_0),
                            &AstNode::AssignProp(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::GetterProp(ref __self_0),
                            &AstNode::GetterProp(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::SetterProp(ref __self_0),
                            &AstNode::SetterProp(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::MethodProp(ref __self_0),
                            &AstNode::MethodProp(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::ComputedPropName(ref __self_0),
                            &AstNode::ComputedPropName(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (&AstNode::BlockStmt(ref __self_0), &AstNode::BlockStmt(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::ExprStmt(ref __self_0), &AstNode::ExprStmt(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::EmptyStmt(ref __self_0), &AstNode::EmptyStmt(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (
                            &AstNode::DebuggerStmt(ref __self_0),
                            &AstNode::DebuggerStmt(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (&AstNode::WithStmt(ref __self_0), &AstNode::WithStmt(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (
                            &AstNode::ReturnStmt(ref __self_0),
                            &AstNode::ReturnStmt(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::LabeledStmt(ref __self_0),
                            &AstNode::LabeledStmt(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (&AstNode::BreakStmt(ref __self_0), &AstNode::BreakStmt(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (
                            &AstNode::ContinueStmt(ref __self_0),
                            &AstNode::ContinueStmt(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (&AstNode::IfStmt(ref __self_0), &AstNode::IfStmt(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (
                            &AstNode::SwitchStmt(ref __self_0),
                            &AstNode::SwitchStmt(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (&AstNode::ThrowStmt(ref __self_0), &AstNode::ThrowStmt(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::TryStmt(ref __self_0), &AstNode::TryStmt(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::WhileStmt(ref __self_0), &AstNode::WhileStmt(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (
                            &AstNode::DoWhileStmt(ref __self_0),
                            &AstNode::DoWhileStmt(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (&AstNode::ForStmt(ref __self_0), &AstNode::ForStmt(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::ForInStmt(ref __self_0), &AstNode::ForInStmt(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (&AstNode::ForOfStmt(ref __self_0), &AstNode::ForOfStmt(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (
                            &AstNode::SwitchCase(ref __self_0),
                            &AstNode::SwitchCase(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::CatchClause(ref __self_0),
                            &AstNode::CatchClause(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (&AstNode::TsTypeAnn(ref __self_0), &AstNode::TsTypeAnn(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (
                            &AstNode::TsTypeParamDecl(ref __self_0),
                            &AstNode::TsTypeParamDecl(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsTypeParam(ref __self_0),
                            &AstNode::TsTypeParam(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsTypeParamInstantiation(ref __self_0),
                            &AstNode::TsTypeParamInstantiation(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsParamProp(ref __self_0),
                            &AstNode::TsParamProp(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsQualifiedName(ref __self_0),
                            &AstNode::TsQualifiedName(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsCallSignatureDecl(ref __self_0),
                            &AstNode::TsCallSignatureDecl(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsConstructSignatureDecl(ref __self_0),
                            &AstNode::TsConstructSignatureDecl(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsPropertySignature(ref __self_0),
                            &AstNode::TsPropertySignature(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsGetterSignature(ref __self_0),
                            &AstNode::TsGetterSignature(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsSetterSignature(ref __self_0),
                            &AstNode::TsSetterSignature(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsMethodSignature(ref __self_0),
                            &AstNode::TsMethodSignature(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsIndexSignature(ref __self_0),
                            &AstNode::TsIndexSignature(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsKeywordType(ref __self_0),
                            &AstNode::TsKeywordType(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsThisType(ref __self_0),
                            &AstNode::TsThisType(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (&AstNode::TsFnType(ref __self_0), &AstNode::TsFnType(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (
                            &AstNode::TsConstructorType(ref __self_0),
                            &AstNode::TsConstructorType(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (&AstNode::TsTypeRef(ref __self_0), &AstNode::TsTypeRef(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (
                            &AstNode::TsTypePredicate(ref __self_0),
                            &AstNode::TsTypePredicate(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsTypeQuery(ref __self_0),
                            &AstNode::TsTypeQuery(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsImportType(ref __self_0),
                            &AstNode::TsImportType(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (&AstNode::TsTypeLit(ref __self_0), &AstNode::TsTypeLit(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (
                            &AstNode::TsArrayType(ref __self_0),
                            &AstNode::TsArrayType(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsTupleType(ref __self_0),
                            &AstNode::TsTupleType(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsTupleElement(ref __self_0),
                            &AstNode::TsTupleElement(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsOptionalType(ref __self_0),
                            &AstNode::TsOptionalType(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsRestType(ref __self_0),
                            &AstNode::TsRestType(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsUnionType(ref __self_0),
                            &AstNode::TsUnionType(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsIntersectionType(ref __self_0),
                            &AstNode::TsIntersectionType(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsConditionalType(ref __self_0),
                            &AstNode::TsConditionalType(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsInferType(ref __self_0),
                            &AstNode::TsInferType(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsParenthesizedType(ref __self_0),
                            &AstNode::TsParenthesizedType(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsTypeOperator(ref __self_0),
                            &AstNode::TsTypeOperator(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsIndexedAccessType(ref __self_0),
                            &AstNode::TsIndexedAccessType(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsMappedType(ref __self_0),
                            &AstNode::TsMappedType(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (&AstNode::TsLitType(ref __self_0), &AstNode::TsLitType(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (
                            &AstNode::TsTplLitType(ref __self_0),
                            &AstNode::TsTplLitType(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsInterfaceDecl(ref __self_0),
                            &AstNode::TsInterfaceDecl(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsInterfaceBody(ref __self_0),
                            &AstNode::TsInterfaceBody(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsExprWithTypeArgs(ref __self_0),
                            &AstNode::TsExprWithTypeArgs(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsTypeAliasDecl(ref __self_0),
                            &AstNode::TsTypeAliasDecl(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsEnumDecl(ref __self_0),
                            &AstNode::TsEnumDecl(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsEnumMember(ref __self_0),
                            &AstNode::TsEnumMember(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsModuleDecl(ref __self_0),
                            &AstNode::TsModuleDecl(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsModuleBlock(ref __self_0),
                            &AstNode::TsModuleBlock(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsNamespaceDecl(ref __self_0),
                            &AstNode::TsNamespaceDecl(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsImportEqualsDecl(ref __self_0),
                            &AstNode::TsImportEqualsDecl(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsExternalModuleRef(ref __self_0),
                            &AstNode::TsExternalModuleRef(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsExportAssignment(ref __self_0),
                            &AstNode::TsExportAssignment(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsNamespaceExportDecl(ref __self_0),
                            &AstNode::TsNamespaceExportDecl(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (&AstNode::TsAsExpr(ref __self_0), &AstNode::TsAsExpr(ref __arg_1_0)) => {
                            (*__self_0) != (*__arg_1_0)
                        }
                        (
                            &AstNode::TsTypeAssertion(ref __self_0),
                            &AstNode::TsTypeAssertion(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsNonNullExpr(ref __self_0),
                            &AstNode::TsNonNullExpr(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        (
                            &AstNode::TsConstAssertion(ref __self_0),
                            &AstNode::TsConstAssertion(ref __arg_1_0),
                        ) => (*__self_0) != (*__arg_1_0),
                        _ => unsafe { ::core::intrinsics::unreachable() },
                    }
                } else {
                    true
                }
            }
        }
    }
    impl<'ast> ::core::marker::StructuralEq for AstNode<'ast> {}
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl<'ast> ::core::cmp::Eq for AstNode<'ast> {
        #[inline]
        #[doc(hidden)]
        fn assert_receiver_is_total_eq(&self) -> () {
            {
                let _: ::core::cmp::AssertParamIsEq<&'ast Class>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ClassProp>;
                let _: ::core::cmp::AssertParamIsEq<&'ast PrivateProp>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ClassMethod>;
                let _: ::core::cmp::AssertParamIsEq<&'ast PrivateMethod>;
                let _: ::core::cmp::AssertParamIsEq<&'ast Constructor>;
                let _: ::core::cmp::AssertParamIsEq<&'ast Decorator>;
                let _: ::core::cmp::AssertParamIsEq<&'ast FnDecl>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ClassDecl>;
                let _: ::core::cmp::AssertParamIsEq<&'ast VarDecl>;
                let _: ::core::cmp::AssertParamIsEq<&'ast VarDeclarator>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ThisExpr>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ArrayLit>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ObjectLit>;
                let _: ::core::cmp::AssertParamIsEq<&'ast SpreadElement>;
                let _: ::core::cmp::AssertParamIsEq<&'ast UnaryExpr>;
                let _: ::core::cmp::AssertParamIsEq<&'ast UpdateExpr>;
                let _: ::core::cmp::AssertParamIsEq<&'ast BinExpr>;
                let _: ::core::cmp::AssertParamIsEq<&'ast FnExpr>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ClassExpr>;
                let _: ::core::cmp::AssertParamIsEq<&'ast AssignExpr>;
                let _: ::core::cmp::AssertParamIsEq<&'ast MemberExpr>;
                let _: ::core::cmp::AssertParamIsEq<&'ast CondExpr>;
                let _: ::core::cmp::AssertParamIsEq<&'ast CallExpr>;
                let _: ::core::cmp::AssertParamIsEq<&'ast NewExpr>;
                let _: ::core::cmp::AssertParamIsEq<&'ast SeqExpr>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ArrowExpr>;
                let _: ::core::cmp::AssertParamIsEq<&'ast YieldExpr>;
                let _: ::core::cmp::AssertParamIsEq<&'ast MetaPropExpr>;
                let _: ::core::cmp::AssertParamIsEq<&'ast AwaitExpr>;
                let _: ::core::cmp::AssertParamIsEq<&'ast Tpl>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TaggedTpl>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TplElement>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ParenExpr>;
                let _: ::core::cmp::AssertParamIsEq<&'ast Super>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ExprOrSpread>;
                let _: ::core::cmp::AssertParamIsEq<&'ast OptChainExpr>;
                let _: ::core::cmp::AssertParamIsEq<&'ast Function>;
                let _: ::core::cmp::AssertParamIsEq<&'ast Param>;
                let _: ::core::cmp::AssertParamIsEq<&'ast BindingIdent>;
                let _: ::core::cmp::AssertParamIsEq<&'ast Ident>;
                let _: ::core::cmp::AssertParamIsEq<&'ast PrivateName>;
                let _: ::core::cmp::AssertParamIsEq<&'ast JSXMemberExpr>;
                let _: ::core::cmp::AssertParamIsEq<&'ast JSXNamespacedName>;
                let _: ::core::cmp::AssertParamIsEq<&'ast JSXEmptyExpr>;
                let _: ::core::cmp::AssertParamIsEq<&'ast JSXExprContainer>;
                let _: ::core::cmp::AssertParamIsEq<&'ast JSXSpreadChild>;
                let _: ::core::cmp::AssertParamIsEq<&'ast JSXOpeningElement>;
                let _: ::core::cmp::AssertParamIsEq<&'ast JSXClosingElement>;
                let _: ::core::cmp::AssertParamIsEq<&'ast JSXAttr>;
                let _: ::core::cmp::AssertParamIsEq<&'ast JSXText>;
                let _: ::core::cmp::AssertParamIsEq<&'ast JSXElement>;
                let _: ::core::cmp::AssertParamIsEq<&'ast JSXFragment>;
                let _: ::core::cmp::AssertParamIsEq<&'ast JSXOpeningFragment>;
                let _: ::core::cmp::AssertParamIsEq<&'ast JSXClosingFragment>;
                let _: ::core::cmp::AssertParamIsEq<&'ast Invalid>;
                let _: ::core::cmp::AssertParamIsEq<&'ast Str>;
                let _: ::core::cmp::AssertParamIsEq<&'ast Bool>;
                let _: ::core::cmp::AssertParamIsEq<&'ast Null>;
                let _: ::core::cmp::AssertParamIsEq<&'ast Number>;
                let _: ::core::cmp::AssertParamIsEq<&'ast BigInt>;
                let _: ::core::cmp::AssertParamIsEq<&'ast Regex>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ExportDefaultExpr>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ExportDecl>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ImportDecl>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ExportAll>;
                let _: ::core::cmp::AssertParamIsEq<&'ast NamedExport>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ExportDefaultDecl>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ImportDefaultSpecifier>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ImportStarAsSpecifier>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ImportNamedSpecifier>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ExportNamespaceSpecifier>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ExportDefaultSpecifier>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ExportNamedSpecifier>;
                let _: ::core::cmp::AssertParamIsEq<&'ast Script>;
                let _: ::core::cmp::AssertParamIsEq<&'ast Module>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ArrayPat>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ObjectPat>;
                let _: ::core::cmp::AssertParamIsEq<&'ast AssignPat>;
                let _: ::core::cmp::AssertParamIsEq<&'ast RestPat>;
                let _: ::core::cmp::AssertParamIsEq<&'ast KeyValuePatProp>;
                let _: ::core::cmp::AssertParamIsEq<&'ast AssignPatProp>;
                let _: ::core::cmp::AssertParamIsEq<&'ast KeyValueProp>;
                let _: ::core::cmp::AssertParamIsEq<&'ast AssignProp>;
                let _: ::core::cmp::AssertParamIsEq<&'ast GetterProp>;
                let _: ::core::cmp::AssertParamIsEq<&'ast SetterProp>;
                let _: ::core::cmp::AssertParamIsEq<&'ast MethodProp>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ComputedPropName>;
                let _: ::core::cmp::AssertParamIsEq<&'ast BlockStmt>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ExprStmt>;
                let _: ::core::cmp::AssertParamIsEq<&'ast EmptyStmt>;
                let _: ::core::cmp::AssertParamIsEq<&'ast DebuggerStmt>;
                let _: ::core::cmp::AssertParamIsEq<&'ast WithStmt>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ReturnStmt>;
                let _: ::core::cmp::AssertParamIsEq<&'ast LabeledStmt>;
                let _: ::core::cmp::AssertParamIsEq<&'ast BreakStmt>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ContinueStmt>;
                let _: ::core::cmp::AssertParamIsEq<&'ast IfStmt>;
                let _: ::core::cmp::AssertParamIsEq<&'ast SwitchStmt>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ThrowStmt>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TryStmt>;
                let _: ::core::cmp::AssertParamIsEq<&'ast WhileStmt>;
                let _: ::core::cmp::AssertParamIsEq<&'ast DoWhileStmt>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ForStmt>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ForInStmt>;
                let _: ::core::cmp::AssertParamIsEq<&'ast ForOfStmt>;
                let _: ::core::cmp::AssertParamIsEq<&'ast SwitchCase>;
                let _: ::core::cmp::AssertParamIsEq<&'ast CatchClause>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsTypeAnn>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsTypeParamDecl>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsTypeParam>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsTypeParamInstantiation>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsParamProp>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsQualifiedName>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsCallSignatureDecl>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsConstructSignatureDecl>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsPropertySignature>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsGetterSignature>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsSetterSignature>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsMethodSignature>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsIndexSignature>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsKeywordType>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsThisType>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsFnType>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsConstructorType>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsTypeRef>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsTypePredicate>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsTypeQuery>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsImportType>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsTypeLit>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsArrayType>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsTupleType>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsTupleElement>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsOptionalType>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsRestType>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsUnionType>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsIntersectionType>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsConditionalType>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsInferType>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsParenthesizedType>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsTypeOperator>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsIndexedAccessType>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsMappedType>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsLitType>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsTplLitType>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsInterfaceDecl>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsInterfaceBody>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsExprWithTypeArgs>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsTypeAliasDecl>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsEnumDecl>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsEnumMember>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsModuleDecl>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsModuleBlock>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsNamespaceDecl>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsImportEqualsDecl>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsExternalModuleRef>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsExportAssignment>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsNamespaceExportDecl>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsAsExpr>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsTypeAssertion>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsNonNullExpr>;
                let _: ::core::cmp::AssertParamIsEq<&'ast TsConstAssertion>;
            }
        }
    }
    #[automatically_derived]
    #[allow(unused_qualifications)]
    impl<'ast> ::core::hash::Hash for AstNode<'ast> {
        fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
            match (&*self,) {
                (&AstNode::Class(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ClassProp(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::PrivateProp(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ClassMethod(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::PrivateMethod(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::Constructor(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::Decorator(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::FnDecl(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ClassDecl(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::VarDecl(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::VarDeclarator(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ThisExpr(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ArrayLit(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ObjectLit(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::SpreadElement(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::UnaryExpr(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::UpdateExpr(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::BinExpr(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::FnExpr(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ClassExpr(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::AssignExpr(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::MemberExpr(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::CondExpr(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::CallExpr(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::NewExpr(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::SeqExpr(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ArrowExpr(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::YieldExpr(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::MetaPropExpr(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::AwaitExpr(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::Tpl(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TaggedTpl(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TplElement(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ParenExpr(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::Super(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ExprOrSpread(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::OptChainExpr(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::Function(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::Param(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::BindingIdent(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::Ident(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::PrivateName(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::JSXMemberExpr(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::JSXNamespacedName(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::JSXEmptyExpr(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::JSXExprContainer(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::JSXSpreadChild(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::JSXOpeningElement(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::JSXClosingElement(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::JSXAttr(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::JSXText(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::JSXElement(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::JSXFragment(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::JSXOpeningFragment(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::JSXClosingFragment(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::Invalid(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::Str(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::Bool(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::Null(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::Number(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::BigInt(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::Regex(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ExportDefaultExpr(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ExportDecl(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ImportDecl(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ExportAll(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::NamedExport(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ExportDefaultDecl(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ImportDefaultSpecifier(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ImportStarAsSpecifier(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ImportNamedSpecifier(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ExportNamespaceSpecifier(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ExportDefaultSpecifier(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ExportNamedSpecifier(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::Script(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::Module(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ArrayPat(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ObjectPat(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::AssignPat(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::RestPat(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::KeyValuePatProp(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::AssignPatProp(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::KeyValueProp(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::AssignProp(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::GetterProp(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::SetterProp(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::MethodProp(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ComputedPropName(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::BlockStmt(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ExprStmt(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::EmptyStmt(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::DebuggerStmt(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::WithStmt(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ReturnStmt(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::LabeledStmt(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::BreakStmt(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ContinueStmt(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::IfStmt(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::SwitchStmt(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ThrowStmt(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TryStmt(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::WhileStmt(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::DoWhileStmt(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ForStmt(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ForInStmt(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::ForOfStmt(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::SwitchCase(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::CatchClause(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsTypeAnn(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsTypeParamDecl(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsTypeParam(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsTypeParamInstantiation(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsParamProp(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsQualifiedName(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsCallSignatureDecl(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsConstructSignatureDecl(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsPropertySignature(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsGetterSignature(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsSetterSignature(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsMethodSignature(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsIndexSignature(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsKeywordType(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsThisType(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsFnType(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsConstructorType(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsTypeRef(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsTypePredicate(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsTypeQuery(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsImportType(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsTypeLit(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsArrayType(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsTupleType(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsTupleElement(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsOptionalType(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsRestType(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsUnionType(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsIntersectionType(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsConditionalType(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsInferType(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsParenthesizedType(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsTypeOperator(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsIndexedAccessType(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsMappedType(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsLitType(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsTplLitType(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsInterfaceDecl(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsInterfaceBody(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsExprWithTypeArgs(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsTypeAliasDecl(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsEnumDecl(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsEnumMember(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsModuleDecl(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsModuleBlock(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsNamespaceDecl(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsImportEqualsDecl(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsExternalModuleRef(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsExportAssignment(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsNamespaceExportDecl(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsAsExpr(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsTypeAssertion(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsNonNullExpr(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
                (&AstNode::TsConstAssertion(ref __self_0),) => {
                    ::core::hash::Hash::hash(&::core::intrinsics::discriminant_value(self), state);
                    ::core::hash::Hash::hash(&(*__self_0), state)
                }
            }
        }
    }
    impl<'ast> From<&'ast Class> for AstNode<'ast> {
        fn from(other: &'ast Class) -> AstNode<'ast> {
            AstNode::Class(other)
        }
    }
    impl<'ast> From<&'ast ClassProp> for AstNode<'ast> {
        fn from(other: &'ast ClassProp) -> AstNode<'ast> {
            AstNode::ClassProp(other)
        }
    }
    impl<'ast> From<&'ast PrivateProp> for AstNode<'ast> {
        fn from(other: &'ast PrivateProp) -> AstNode<'ast> {
            AstNode::PrivateProp(other)
        }
    }
    impl<'ast> From<&'ast ClassMethod> for AstNode<'ast> {
        fn from(other: &'ast ClassMethod) -> AstNode<'ast> {
            AstNode::ClassMethod(other)
        }
    }
    impl<'ast> From<&'ast PrivateMethod> for AstNode<'ast> {
        fn from(other: &'ast PrivateMethod) -> AstNode<'ast> {
            AstNode::PrivateMethod(other)
        }
    }
    impl<'ast> From<&'ast Constructor> for AstNode<'ast> {
        fn from(other: &'ast Constructor) -> AstNode<'ast> {
            AstNode::Constructor(other)
        }
    }
    impl<'ast> From<&'ast Decorator> for AstNode<'ast> {
        fn from(other: &'ast Decorator) -> AstNode<'ast> {
            AstNode::Decorator(other)
        }
    }
    impl<'ast> From<&'ast FnDecl> for AstNode<'ast> {
        fn from(other: &'ast FnDecl) -> AstNode<'ast> {
            AstNode::FnDecl(other)
        }
    }
    impl<'ast> From<&'ast ClassDecl> for AstNode<'ast> {
        fn from(other: &'ast ClassDecl) -> AstNode<'ast> {
            AstNode::ClassDecl(other)
        }
    }
    impl<'ast> From<&'ast VarDecl> for AstNode<'ast> {
        fn from(other: &'ast VarDecl) -> AstNode<'ast> {
            AstNode::VarDecl(other)
        }
    }
    impl<'ast> From<&'ast VarDeclarator> for AstNode<'ast> {
        fn from(other: &'ast VarDeclarator) -> AstNode<'ast> {
            AstNode::VarDeclarator(other)
        }
    }
    impl<'ast> From<&'ast ThisExpr> for AstNode<'ast> {
        fn from(other: &'ast ThisExpr) -> AstNode<'ast> {
            AstNode::ThisExpr(other)
        }
    }
    impl<'ast> From<&'ast ArrayLit> for AstNode<'ast> {
        fn from(other: &'ast ArrayLit) -> AstNode<'ast> {
            AstNode::ArrayLit(other)
        }
    }
    impl<'ast> From<&'ast ObjectLit> for AstNode<'ast> {
        fn from(other: &'ast ObjectLit) -> AstNode<'ast> {
            AstNode::ObjectLit(other)
        }
    }
    impl<'ast> From<&'ast SpreadElement> for AstNode<'ast> {
        fn from(other: &'ast SpreadElement) -> AstNode<'ast> {
            AstNode::SpreadElement(other)
        }
    }
    impl<'ast> From<&'ast UnaryExpr> for AstNode<'ast> {
        fn from(other: &'ast UnaryExpr) -> AstNode<'ast> {
            AstNode::UnaryExpr(other)
        }
    }
    impl<'ast> From<&'ast UpdateExpr> for AstNode<'ast> {
        fn from(other: &'ast UpdateExpr) -> AstNode<'ast> {
            AstNode::UpdateExpr(other)
        }
    }
    impl<'ast> From<&'ast BinExpr> for AstNode<'ast> {
        fn from(other: &'ast BinExpr) -> AstNode<'ast> {
            AstNode::BinExpr(other)
        }
    }
    impl<'ast> From<&'ast FnExpr> for AstNode<'ast> {
        fn from(other: &'ast FnExpr) -> AstNode<'ast> {
            AstNode::FnExpr(other)
        }
    }
    impl<'ast> From<&'ast ClassExpr> for AstNode<'ast> {
        fn from(other: &'ast ClassExpr) -> AstNode<'ast> {
            AstNode::ClassExpr(other)
        }
    }
    impl<'ast> From<&'ast AssignExpr> for AstNode<'ast> {
        fn from(other: &'ast AssignExpr) -> AstNode<'ast> {
            AstNode::AssignExpr(other)
        }
    }
    impl<'ast> From<&'ast MemberExpr> for AstNode<'ast> {
        fn from(other: &'ast MemberExpr) -> AstNode<'ast> {
            AstNode::MemberExpr(other)
        }
    }
    impl<'ast> From<&'ast CondExpr> for AstNode<'ast> {
        fn from(other: &'ast CondExpr) -> AstNode<'ast> {
            AstNode::CondExpr(other)
        }
    }
    impl<'ast> From<&'ast CallExpr> for AstNode<'ast> {
        fn from(other: &'ast CallExpr) -> AstNode<'ast> {
            AstNode::CallExpr(other)
        }
    }
    impl<'ast> From<&'ast NewExpr> for AstNode<'ast> {
        fn from(other: &'ast NewExpr) -> AstNode<'ast> {
            AstNode::NewExpr(other)
        }
    }
    impl<'ast> From<&'ast SeqExpr> for AstNode<'ast> {
        fn from(other: &'ast SeqExpr) -> AstNode<'ast> {
            AstNode::SeqExpr(other)
        }
    }
    impl<'ast> From<&'ast ArrowExpr> for AstNode<'ast> {
        fn from(other: &'ast ArrowExpr) -> AstNode<'ast> {
            AstNode::ArrowExpr(other)
        }
    }
    impl<'ast> From<&'ast YieldExpr> for AstNode<'ast> {
        fn from(other: &'ast YieldExpr) -> AstNode<'ast> {
            AstNode::YieldExpr(other)
        }
    }
    impl<'ast> From<&'ast MetaPropExpr> for AstNode<'ast> {
        fn from(other: &'ast MetaPropExpr) -> AstNode<'ast> {
            AstNode::MetaPropExpr(other)
        }
    }
    impl<'ast> From<&'ast AwaitExpr> for AstNode<'ast> {
        fn from(other: &'ast AwaitExpr) -> AstNode<'ast> {
            AstNode::AwaitExpr(other)
        }
    }
    impl<'ast> From<&'ast Tpl> for AstNode<'ast> {
        fn from(other: &'ast Tpl) -> AstNode<'ast> {
            AstNode::Tpl(other)
        }
    }
    impl<'ast> From<&'ast TaggedTpl> for AstNode<'ast> {
        fn from(other: &'ast TaggedTpl) -> AstNode<'ast> {
            AstNode::TaggedTpl(other)
        }
    }
    impl<'ast> From<&'ast TplElement> for AstNode<'ast> {
        fn from(other: &'ast TplElement) -> AstNode<'ast> {
            AstNode::TplElement(other)
        }
    }
    impl<'ast> From<&'ast ParenExpr> for AstNode<'ast> {
        fn from(other: &'ast ParenExpr) -> AstNode<'ast> {
            AstNode::ParenExpr(other)
        }
    }
    impl<'ast> From<&'ast Super> for AstNode<'ast> {
        fn from(other: &'ast Super) -> AstNode<'ast> {
            AstNode::Super(other)
        }
    }
    impl<'ast> From<&'ast ExprOrSpread> for AstNode<'ast> {
        fn from(other: &'ast ExprOrSpread) -> AstNode<'ast> {
            AstNode::ExprOrSpread(other)
        }
    }
    impl<'ast> From<&'ast OptChainExpr> for AstNode<'ast> {
        fn from(other: &'ast OptChainExpr) -> AstNode<'ast> {
            AstNode::OptChainExpr(other)
        }
    }
    impl<'ast> From<&'ast Function> for AstNode<'ast> {
        fn from(other: &'ast Function) -> AstNode<'ast> {
            AstNode::Function(other)
        }
    }
    impl<'ast> From<&'ast Param> for AstNode<'ast> {
        fn from(other: &'ast Param) -> AstNode<'ast> {
            AstNode::Param(other)
        }
    }
    impl<'ast> From<&'ast BindingIdent> for AstNode<'ast> {
        fn from(other: &'ast BindingIdent) -> AstNode<'ast> {
            AstNode::BindingIdent(other)
        }
    }
    impl<'ast> From<&'ast Ident> for AstNode<'ast> {
        fn from(other: &'ast Ident) -> AstNode<'ast> {
            AstNode::Ident(other)
        }
    }
    impl<'ast> From<&'ast PrivateName> for AstNode<'ast> {
        fn from(other: &'ast PrivateName) -> AstNode<'ast> {
            AstNode::PrivateName(other)
        }
    }
    impl<'ast> From<&'ast JSXMemberExpr> for AstNode<'ast> {
        fn from(other: &'ast JSXMemberExpr) -> AstNode<'ast> {
            AstNode::JSXMemberExpr(other)
        }
    }
    impl<'ast> From<&'ast JSXNamespacedName> for AstNode<'ast> {
        fn from(other: &'ast JSXNamespacedName) -> AstNode<'ast> {
            AstNode::JSXNamespacedName(other)
        }
    }
    impl<'ast> From<&'ast JSXEmptyExpr> for AstNode<'ast> {
        fn from(other: &'ast JSXEmptyExpr) -> AstNode<'ast> {
            AstNode::JSXEmptyExpr(other)
        }
    }
    impl<'ast> From<&'ast JSXExprContainer> for AstNode<'ast> {
        fn from(other: &'ast JSXExprContainer) -> AstNode<'ast> {
            AstNode::JSXExprContainer(other)
        }
    }
    impl<'ast> From<&'ast JSXSpreadChild> for AstNode<'ast> {
        fn from(other: &'ast JSXSpreadChild) -> AstNode<'ast> {
            AstNode::JSXSpreadChild(other)
        }
    }
    impl<'ast> From<&'ast JSXOpeningElement> for AstNode<'ast> {
        fn from(other: &'ast JSXOpeningElement) -> AstNode<'ast> {
            AstNode::JSXOpeningElement(other)
        }
    }
    impl<'ast> From<&'ast JSXClosingElement> for AstNode<'ast> {
        fn from(other: &'ast JSXClosingElement) -> AstNode<'ast> {
            AstNode::JSXClosingElement(other)
        }
    }
    impl<'ast> From<&'ast JSXAttr> for AstNode<'ast> {
        fn from(other: &'ast JSXAttr) -> AstNode<'ast> {
            AstNode::JSXAttr(other)
        }
    }
    impl<'ast> From<&'ast JSXText> for AstNode<'ast> {
        fn from(other: &'ast JSXText) -> AstNode<'ast> {
            AstNode::JSXText(other)
        }
    }
    impl<'ast> From<&'ast JSXElement> for AstNode<'ast> {
        fn from(other: &'ast JSXElement) -> AstNode<'ast> {
            AstNode::JSXElement(other)
        }
    }
    impl<'ast> From<&'ast JSXFragment> for AstNode<'ast> {
        fn from(other: &'ast JSXFragment) -> AstNode<'ast> {
            AstNode::JSXFragment(other)
        }
    }
    impl<'ast> From<&'ast JSXOpeningFragment> for AstNode<'ast> {
        fn from(other: &'ast JSXOpeningFragment) -> AstNode<'ast> {
            AstNode::JSXOpeningFragment(other)
        }
    }
    impl<'ast> From<&'ast JSXClosingFragment> for AstNode<'ast> {
        fn from(other: &'ast JSXClosingFragment) -> AstNode<'ast> {
            AstNode::JSXClosingFragment(other)
        }
    }
    impl<'ast> From<&'ast Invalid> for AstNode<'ast> {
        fn from(other: &'ast Invalid) -> AstNode<'ast> {
            AstNode::Invalid(other)
        }
    }
    impl<'ast> From<&'ast Str> for AstNode<'ast> {
        fn from(other: &'ast Str) -> AstNode<'ast> {
            AstNode::Str(other)
        }
    }
    impl<'ast> From<&'ast Bool> for AstNode<'ast> {
        fn from(other: &'ast Bool) -> AstNode<'ast> {
            AstNode::Bool(other)
        }
    }
    impl<'ast> From<&'ast Null> for AstNode<'ast> {
        fn from(other: &'ast Null) -> AstNode<'ast> {
            AstNode::Null(other)
        }
    }
    impl<'ast> From<&'ast Number> for AstNode<'ast> {
        fn from(other: &'ast Number) -> AstNode<'ast> {
            AstNode::Number(other)
        }
    }
    impl<'ast> From<&'ast BigInt> for AstNode<'ast> {
        fn from(other: &'ast BigInt) -> AstNode<'ast> {
            AstNode::BigInt(other)
        }
    }
    impl<'ast> From<&'ast Regex> for AstNode<'ast> {
        fn from(other: &'ast Regex) -> AstNode<'ast> {
            AstNode::Regex(other)
        }
    }
    impl<'ast> From<&'ast ExportDefaultExpr> for AstNode<'ast> {
        fn from(other: &'ast ExportDefaultExpr) -> AstNode<'ast> {
            AstNode::ExportDefaultExpr(other)
        }
    }
    impl<'ast> From<&'ast ExportDecl> for AstNode<'ast> {
        fn from(other: &'ast ExportDecl) -> AstNode<'ast> {
            AstNode::ExportDecl(other)
        }
    }
    impl<'ast> From<&'ast ImportDecl> for AstNode<'ast> {
        fn from(other: &'ast ImportDecl) -> AstNode<'ast> {
            AstNode::ImportDecl(other)
        }
    }
    impl<'ast> From<&'ast ExportAll> for AstNode<'ast> {
        fn from(other: &'ast ExportAll) -> AstNode<'ast> {
            AstNode::ExportAll(other)
        }
    }
    impl<'ast> From<&'ast NamedExport> for AstNode<'ast> {
        fn from(other: &'ast NamedExport) -> AstNode<'ast> {
            AstNode::NamedExport(other)
        }
    }
    impl<'ast> From<&'ast ExportDefaultDecl> for AstNode<'ast> {
        fn from(other: &'ast ExportDefaultDecl) -> AstNode<'ast> {
            AstNode::ExportDefaultDecl(other)
        }
    }
    impl<'ast> From<&'ast ImportDefaultSpecifier> for AstNode<'ast> {
        fn from(other: &'ast ImportDefaultSpecifier) -> AstNode<'ast> {
            AstNode::ImportDefaultSpecifier(other)
        }
    }
    impl<'ast> From<&'ast ImportStarAsSpecifier> for AstNode<'ast> {
        fn from(other: &'ast ImportStarAsSpecifier) -> AstNode<'ast> {
            AstNode::ImportStarAsSpecifier(other)
        }
    }
    impl<'ast> From<&'ast ImportNamedSpecifier> for AstNode<'ast> {
        fn from(other: &'ast ImportNamedSpecifier) -> AstNode<'ast> {
            AstNode::ImportNamedSpecifier(other)
        }
    }
    impl<'ast> From<&'ast ExportNamespaceSpecifier> for AstNode<'ast> {
        fn from(other: &'ast ExportNamespaceSpecifier) -> AstNode<'ast> {
            AstNode::ExportNamespaceSpecifier(other)
        }
    }
    impl<'ast> From<&'ast ExportDefaultSpecifier> for AstNode<'ast> {
        fn from(other: &'ast ExportDefaultSpecifier) -> AstNode<'ast> {
            AstNode::ExportDefaultSpecifier(other)
        }
    }
    impl<'ast> From<&'ast ExportNamedSpecifier> for AstNode<'ast> {
        fn from(other: &'ast ExportNamedSpecifier) -> AstNode<'ast> {
            AstNode::ExportNamedSpecifier(other)
        }
    }
    impl<'ast> From<&'ast Script> for AstNode<'ast> {
        fn from(other: &'ast Script) -> AstNode<'ast> {
            AstNode::Script(other)
        }
    }
    impl<'ast> From<&'ast Module> for AstNode<'ast> {
        fn from(other: &'ast Module) -> AstNode<'ast> {
            AstNode::Module(other)
        }
    }
    impl<'ast> From<&'ast ArrayPat> for AstNode<'ast> {
        fn from(other: &'ast ArrayPat) -> AstNode<'ast> {
            AstNode::ArrayPat(other)
        }
    }
    impl<'ast> From<&'ast ObjectPat> for AstNode<'ast> {
        fn from(other: &'ast ObjectPat) -> AstNode<'ast> {
            AstNode::ObjectPat(other)
        }
    }
    impl<'ast> From<&'ast AssignPat> for AstNode<'ast> {
        fn from(other: &'ast AssignPat) -> AstNode<'ast> {
            AstNode::AssignPat(other)
        }
    }
    impl<'ast> From<&'ast RestPat> for AstNode<'ast> {
        fn from(other: &'ast RestPat) -> AstNode<'ast> {
            AstNode::RestPat(other)
        }
    }
    impl<'ast> From<&'ast KeyValuePatProp> for AstNode<'ast> {
        fn from(other: &'ast KeyValuePatProp) -> AstNode<'ast> {
            AstNode::KeyValuePatProp(other)
        }
    }
    impl<'ast> From<&'ast AssignPatProp> for AstNode<'ast> {
        fn from(other: &'ast AssignPatProp) -> AstNode<'ast> {
            AstNode::AssignPatProp(other)
        }
    }
    impl<'ast> From<&'ast KeyValueProp> for AstNode<'ast> {
        fn from(other: &'ast KeyValueProp) -> AstNode<'ast> {
            AstNode::KeyValueProp(other)
        }
    }
    impl<'ast> From<&'ast AssignProp> for AstNode<'ast> {
        fn from(other: &'ast AssignProp) -> AstNode<'ast> {
            AstNode::AssignProp(other)
        }
    }
    impl<'ast> From<&'ast GetterProp> for AstNode<'ast> {
        fn from(other: &'ast GetterProp) -> AstNode<'ast> {
            AstNode::GetterProp(other)
        }
    }
    impl<'ast> From<&'ast SetterProp> for AstNode<'ast> {
        fn from(other: &'ast SetterProp) -> AstNode<'ast> {
            AstNode::SetterProp(other)
        }
    }
    impl<'ast> From<&'ast MethodProp> for AstNode<'ast> {
        fn from(other: &'ast MethodProp) -> AstNode<'ast> {
            AstNode::MethodProp(other)
        }
    }
    impl<'ast> From<&'ast ComputedPropName> for AstNode<'ast> {
        fn from(other: &'ast ComputedPropName) -> AstNode<'ast> {
            AstNode::ComputedPropName(other)
        }
    }
    impl<'ast> From<&'ast BlockStmt> for AstNode<'ast> {
        fn from(other: &'ast BlockStmt) -> AstNode<'ast> {
            AstNode::BlockStmt(other)
        }
    }
    impl<'ast> From<&'ast ExprStmt> for AstNode<'ast> {
        fn from(other: &'ast ExprStmt) -> AstNode<'ast> {
            AstNode::ExprStmt(other)
        }
    }
    impl<'ast> From<&'ast EmptyStmt> for AstNode<'ast> {
        fn from(other: &'ast EmptyStmt) -> AstNode<'ast> {
            AstNode::EmptyStmt(other)
        }
    }
    impl<'ast> From<&'ast DebuggerStmt> for AstNode<'ast> {
        fn from(other: &'ast DebuggerStmt) -> AstNode<'ast> {
            AstNode::DebuggerStmt(other)
        }
    }
    impl<'ast> From<&'ast WithStmt> for AstNode<'ast> {
        fn from(other: &'ast WithStmt) -> AstNode<'ast> {
            AstNode::WithStmt(other)
        }
    }
    impl<'ast> From<&'ast ReturnStmt> for AstNode<'ast> {
        fn from(other: &'ast ReturnStmt) -> AstNode<'ast> {
            AstNode::ReturnStmt(other)
        }
    }
    impl<'ast> From<&'ast LabeledStmt> for AstNode<'ast> {
        fn from(other: &'ast LabeledStmt) -> AstNode<'ast> {
            AstNode::LabeledStmt(other)
        }
    }
    impl<'ast> From<&'ast BreakStmt> for AstNode<'ast> {
        fn from(other: &'ast BreakStmt) -> AstNode<'ast> {
            AstNode::BreakStmt(other)
        }
    }
    impl<'ast> From<&'ast ContinueStmt> for AstNode<'ast> {
        fn from(other: &'ast ContinueStmt) -> AstNode<'ast> {
            AstNode::ContinueStmt(other)
        }
    }
    impl<'ast> From<&'ast IfStmt> for AstNode<'ast> {
        fn from(other: &'ast IfStmt) -> AstNode<'ast> {
            AstNode::IfStmt(other)
        }
    }
    impl<'ast> From<&'ast SwitchStmt> for AstNode<'ast> {
        fn from(other: &'ast SwitchStmt) -> AstNode<'ast> {
            AstNode::SwitchStmt(other)
        }
    }
    impl<'ast> From<&'ast ThrowStmt> for AstNode<'ast> {
        fn from(other: &'ast ThrowStmt) -> AstNode<'ast> {
            AstNode::ThrowStmt(other)
        }
    }
    impl<'ast> From<&'ast TryStmt> for AstNode<'ast> {
        fn from(other: &'ast TryStmt) -> AstNode<'ast> {
            AstNode::TryStmt(other)
        }
    }
    impl<'ast> From<&'ast WhileStmt> for AstNode<'ast> {
        fn from(other: &'ast WhileStmt) -> AstNode<'ast> {
            AstNode::WhileStmt(other)
        }
    }
    impl<'ast> From<&'ast DoWhileStmt> for AstNode<'ast> {
        fn from(other: &'ast DoWhileStmt) -> AstNode<'ast> {
            AstNode::DoWhileStmt(other)
        }
    }
    impl<'ast> From<&'ast ForStmt> for AstNode<'ast> {
        fn from(other: &'ast ForStmt) -> AstNode<'ast> {
            AstNode::ForStmt(other)
        }
    }
    impl<'ast> From<&'ast ForInStmt> for AstNode<'ast> {
        fn from(other: &'ast ForInStmt) -> AstNode<'ast> {
            AstNode::ForInStmt(other)
        }
    }
    impl<'ast> From<&'ast ForOfStmt> for AstNode<'ast> {
        fn from(other: &'ast ForOfStmt) -> AstNode<'ast> {
            AstNode::ForOfStmt(other)
        }
    }
    impl<'ast> From<&'ast SwitchCase> for AstNode<'ast> {
        fn from(other: &'ast SwitchCase) -> AstNode<'ast> {
            AstNode::SwitchCase(other)
        }
    }
    impl<'ast> From<&'ast CatchClause> for AstNode<'ast> {
        fn from(other: &'ast CatchClause) -> AstNode<'ast> {
            AstNode::CatchClause(other)
        }
    }
    impl<'ast> From<&'ast TsTypeAnn> for AstNode<'ast> {
        fn from(other: &'ast TsTypeAnn) -> AstNode<'ast> {
            AstNode::TsTypeAnn(other)
        }
    }
    impl<'ast> From<&'ast TsTypeParamDecl> for AstNode<'ast> {
        fn from(other: &'ast TsTypeParamDecl) -> AstNode<'ast> {
            AstNode::TsTypeParamDecl(other)
        }
    }
    impl<'ast> From<&'ast TsTypeParam> for AstNode<'ast> {
        fn from(other: &'ast TsTypeParam) -> AstNode<'ast> {
            AstNode::TsTypeParam(other)
        }
    }
    impl<'ast> From<&'ast TsTypeParamInstantiation> for AstNode<'ast> {
        fn from(other: &'ast TsTypeParamInstantiation) -> AstNode<'ast> {
            AstNode::TsTypeParamInstantiation(other)
        }
    }
    impl<'ast> From<&'ast TsParamProp> for AstNode<'ast> {
        fn from(other: &'ast TsParamProp) -> AstNode<'ast> {
            AstNode::TsParamProp(other)
        }
    }
    impl<'ast> From<&'ast TsQualifiedName> for AstNode<'ast> {
        fn from(other: &'ast TsQualifiedName) -> AstNode<'ast> {
            AstNode::TsQualifiedName(other)
        }
    }
    impl<'ast> From<&'ast TsCallSignatureDecl> for AstNode<'ast> {
        fn from(other: &'ast TsCallSignatureDecl) -> AstNode<'ast> {
            AstNode::TsCallSignatureDecl(other)
        }
    }
    impl<'ast> From<&'ast TsConstructSignatureDecl> for AstNode<'ast> {
        fn from(other: &'ast TsConstructSignatureDecl) -> AstNode<'ast> {
            AstNode::TsConstructSignatureDecl(other)
        }
    }
    impl<'ast> From<&'ast TsPropertySignature> for AstNode<'ast> {
        fn from(other: &'ast TsPropertySignature) -> AstNode<'ast> {
            AstNode::TsPropertySignature(other)
        }
    }
    impl<'ast> From<&'ast TsGetterSignature> for AstNode<'ast> {
        fn from(other: &'ast TsGetterSignature) -> AstNode<'ast> {
            AstNode::TsGetterSignature(other)
        }
    }
    impl<'ast> From<&'ast TsSetterSignature> for AstNode<'ast> {
        fn from(other: &'ast TsSetterSignature) -> AstNode<'ast> {
            AstNode::TsSetterSignature(other)
        }
    }
    impl<'ast> From<&'ast TsMethodSignature> for AstNode<'ast> {
        fn from(other: &'ast TsMethodSignature) -> AstNode<'ast> {
            AstNode::TsMethodSignature(other)
        }
    }
    impl<'ast> From<&'ast TsIndexSignature> for AstNode<'ast> {
        fn from(other: &'ast TsIndexSignature) -> AstNode<'ast> {
            AstNode::TsIndexSignature(other)
        }
    }
    impl<'ast> From<&'ast TsKeywordType> for AstNode<'ast> {
        fn from(other: &'ast TsKeywordType) -> AstNode<'ast> {
            AstNode::TsKeywordType(other)
        }
    }
    impl<'ast> From<&'ast TsThisType> for AstNode<'ast> {
        fn from(other: &'ast TsThisType) -> AstNode<'ast> {
            AstNode::TsThisType(other)
        }
    }
    impl<'ast> From<&'ast TsFnType> for AstNode<'ast> {
        fn from(other: &'ast TsFnType) -> AstNode<'ast> {
            AstNode::TsFnType(other)
        }
    }
    impl<'ast> From<&'ast TsConstructorType> for AstNode<'ast> {
        fn from(other: &'ast TsConstructorType) -> AstNode<'ast> {
            AstNode::TsConstructorType(other)
        }
    }
    impl<'ast> From<&'ast TsTypeRef> for AstNode<'ast> {
        fn from(other: &'ast TsTypeRef) -> AstNode<'ast> {
            AstNode::TsTypeRef(other)
        }
    }
    impl<'ast> From<&'ast TsTypePredicate> for AstNode<'ast> {
        fn from(other: &'ast TsTypePredicate) -> AstNode<'ast> {
            AstNode::TsTypePredicate(other)
        }
    }
    impl<'ast> From<&'ast TsTypeQuery> for AstNode<'ast> {
        fn from(other: &'ast TsTypeQuery) -> AstNode<'ast> {
            AstNode::TsTypeQuery(other)
        }
    }
    impl<'ast> From<&'ast TsImportType> for AstNode<'ast> {
        fn from(other: &'ast TsImportType) -> AstNode<'ast> {
            AstNode::TsImportType(other)
        }
    }
    impl<'ast> From<&'ast TsTypeLit> for AstNode<'ast> {
        fn from(other: &'ast TsTypeLit) -> AstNode<'ast> {
            AstNode::TsTypeLit(other)
        }
    }
    impl<'ast> From<&'ast TsArrayType> for AstNode<'ast> {
        fn from(other: &'ast TsArrayType) -> AstNode<'ast> {
            AstNode::TsArrayType(other)
        }
    }
    impl<'ast> From<&'ast TsTupleType> for AstNode<'ast> {
        fn from(other: &'ast TsTupleType) -> AstNode<'ast> {
            AstNode::TsTupleType(other)
        }
    }
    impl<'ast> From<&'ast TsTupleElement> for AstNode<'ast> {
        fn from(other: &'ast TsTupleElement) -> AstNode<'ast> {
            AstNode::TsTupleElement(other)
        }
    }
    impl<'ast> From<&'ast TsOptionalType> for AstNode<'ast> {
        fn from(other: &'ast TsOptionalType) -> AstNode<'ast> {
            AstNode::TsOptionalType(other)
        }
    }
    impl<'ast> From<&'ast TsRestType> for AstNode<'ast> {
        fn from(other: &'ast TsRestType) -> AstNode<'ast> {
            AstNode::TsRestType(other)
        }
    }
    impl<'ast> From<&'ast TsUnionType> for AstNode<'ast> {
        fn from(other: &'ast TsUnionType) -> AstNode<'ast> {
            AstNode::TsUnionType(other)
        }
    }
    impl<'ast> From<&'ast TsIntersectionType> for AstNode<'ast> {
        fn from(other: &'ast TsIntersectionType) -> AstNode<'ast> {
            AstNode::TsIntersectionType(other)
        }
    }
    impl<'ast> From<&'ast TsConditionalType> for AstNode<'ast> {
        fn from(other: &'ast TsConditionalType) -> AstNode<'ast> {
            AstNode::TsConditionalType(other)
        }
    }
    impl<'ast> From<&'ast TsInferType> for AstNode<'ast> {
        fn from(other: &'ast TsInferType) -> AstNode<'ast> {
            AstNode::TsInferType(other)
        }
    }
    impl<'ast> From<&'ast TsParenthesizedType> for AstNode<'ast> {
        fn from(other: &'ast TsParenthesizedType) -> AstNode<'ast> {
            AstNode::TsParenthesizedType(other)
        }
    }
    impl<'ast> From<&'ast TsTypeOperator> for AstNode<'ast> {
        fn from(other: &'ast TsTypeOperator) -> AstNode<'ast> {
            AstNode::TsTypeOperator(other)
        }
    }
    impl<'ast> From<&'ast TsIndexedAccessType> for AstNode<'ast> {
        fn from(other: &'ast TsIndexedAccessType) -> AstNode<'ast> {
            AstNode::TsIndexedAccessType(other)
        }
    }
    impl<'ast> From<&'ast TsMappedType> for AstNode<'ast> {
        fn from(other: &'ast TsMappedType) -> AstNode<'ast> {
            AstNode::TsMappedType(other)
        }
    }
    impl<'ast> From<&'ast TsLitType> for AstNode<'ast> {
        fn from(other: &'ast TsLitType) -> AstNode<'ast> {
            AstNode::TsLitType(other)
        }
    }
    impl<'ast> From<&'ast TsTplLitType> for AstNode<'ast> {
        fn from(other: &'ast TsTplLitType) -> AstNode<'ast> {
            AstNode::TsTplLitType(other)
        }
    }
    impl<'ast> From<&'ast TsInterfaceDecl> for AstNode<'ast> {
        fn from(other: &'ast TsInterfaceDecl) -> AstNode<'ast> {
            AstNode::TsInterfaceDecl(other)
        }
    }
    impl<'ast> From<&'ast TsInterfaceBody> for AstNode<'ast> {
        fn from(other: &'ast TsInterfaceBody) -> AstNode<'ast> {
            AstNode::TsInterfaceBody(other)
        }
    }
    impl<'ast> From<&'ast TsExprWithTypeArgs> for AstNode<'ast> {
        fn from(other: &'ast TsExprWithTypeArgs) -> AstNode<'ast> {
            AstNode::TsExprWithTypeArgs(other)
        }
    }
    impl<'ast> From<&'ast TsTypeAliasDecl> for AstNode<'ast> {
        fn from(other: &'ast TsTypeAliasDecl) -> AstNode<'ast> {
            AstNode::TsTypeAliasDecl(other)
        }
    }
    impl<'ast> From<&'ast TsEnumDecl> for AstNode<'ast> {
        fn from(other: &'ast TsEnumDecl) -> AstNode<'ast> {
            AstNode::TsEnumDecl(other)
        }
    }
    impl<'ast> From<&'ast TsEnumMember> for AstNode<'ast> {
        fn from(other: &'ast TsEnumMember) -> AstNode<'ast> {
            AstNode::TsEnumMember(other)
        }
    }
    impl<'ast> From<&'ast TsModuleDecl> for AstNode<'ast> {
        fn from(other: &'ast TsModuleDecl) -> AstNode<'ast> {
            AstNode::TsModuleDecl(other)
        }
    }
    impl<'ast> From<&'ast TsModuleBlock> for AstNode<'ast> {
        fn from(other: &'ast TsModuleBlock) -> AstNode<'ast> {
            AstNode::TsModuleBlock(other)
        }
    }
    impl<'ast> From<&'ast TsNamespaceDecl> for AstNode<'ast> {
        fn from(other: &'ast TsNamespaceDecl) -> AstNode<'ast> {
            AstNode::TsNamespaceDecl(other)
        }
    }
    impl<'ast> From<&'ast TsImportEqualsDecl> for AstNode<'ast> {
        fn from(other: &'ast TsImportEqualsDecl) -> AstNode<'ast> {
            AstNode::TsImportEqualsDecl(other)
        }
    }
    impl<'ast> From<&'ast TsExternalModuleRef> for AstNode<'ast> {
        fn from(other: &'ast TsExternalModuleRef) -> AstNode<'ast> {
            AstNode::TsExternalModuleRef(other)
        }
    }
    impl<'ast> From<&'ast TsExportAssignment> for AstNode<'ast> {
        fn from(other: &'ast TsExportAssignment) -> AstNode<'ast> {
            AstNode::TsExportAssignment(other)
        }
    }
    impl<'ast> From<&'ast TsNamespaceExportDecl> for AstNode<'ast> {
        fn from(other: &'ast TsNamespaceExportDecl) -> AstNode<'ast> {
            AstNode::TsNamespaceExportDecl(other)
        }
    }
    impl<'ast> From<&'ast TsAsExpr> for AstNode<'ast> {
        fn from(other: &'ast TsAsExpr) -> AstNode<'ast> {
            AstNode::TsAsExpr(other)
        }
    }
    impl<'ast> From<&'ast TsTypeAssertion> for AstNode<'ast> {
        fn from(other: &'ast TsTypeAssertion) -> AstNode<'ast> {
            AstNode::TsTypeAssertion(other)
        }
    }
    impl<'ast> From<&'ast TsNonNullExpr> for AstNode<'ast> {
        fn from(other: &'ast TsNonNullExpr) -> AstNode<'ast> {
            AstNode::TsNonNullExpr(other)
        }
    }
    impl<'ast> From<&'ast TsConstAssertion> for AstNode<'ast> {
        fn from(other: &'ast TsConstAssertion) -> AstNode<'ast> {
            AstNode::TsConstAssertion(other)
        }
    }
    impl<'ast> fmt::Debug for AstNode<'ast> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let (name, span) = match self {
                AstNode::Class(a) => ("Class", a.span()),
                AstNode::ClassProp(a) => ("ClassProp", a.span()),
                AstNode::PrivateProp(a) => ("PrivateProp", a.span()),
                AstNode::ClassMethod(a) => ("ClassMethod", a.span()),
                AstNode::PrivateMethod(a) => ("PrivateMethod", a.span()),
                AstNode::Constructor(a) => ("Constructor", a.span()),
                AstNode::Decorator(a) => ("Decorator", a.span()),
                AstNode::FnDecl(a) => ("FnDecl", a.span()),
                AstNode::ClassDecl(a) => ("ClassDecl", a.span()),
                AstNode::VarDecl(a) => ("VarDecl", a.span()),
                AstNode::VarDeclarator(a) => ("VarDeclarator", a.span()),
                AstNode::ThisExpr(a) => ("ThisExpr", a.span()),
                AstNode::ArrayLit(a) => ("ArrayLit", a.span()),
                AstNode::ObjectLit(a) => ("ObjectLit", a.span()),
                AstNode::SpreadElement(a) => ("SpreadElement", a.span()),
                AstNode::UnaryExpr(a) => ("UnaryExpr", a.span()),
                AstNode::UpdateExpr(a) => ("UpdateExpr", a.span()),
                AstNode::BinExpr(a) => ("BinExpr", a.span()),
                AstNode::FnExpr(a) => ("FnExpr", a.span()),
                AstNode::ClassExpr(a) => ("ClassExpr", a.span()),
                AstNode::AssignExpr(a) => ("AssignExpr", a.span()),
                AstNode::MemberExpr(a) => ("MemberExpr", a.span()),
                AstNode::CondExpr(a) => ("CondExpr", a.span()),
                AstNode::CallExpr(a) => ("CallExpr", a.span()),
                AstNode::NewExpr(a) => ("NewExpr", a.span()),
                AstNode::SeqExpr(a) => ("SeqExpr", a.span()),
                AstNode::ArrowExpr(a) => ("ArrowExpr", a.span()),
                AstNode::YieldExpr(a) => ("YieldExpr", a.span()),
                AstNode::MetaPropExpr(a) => ("MetaPropExpr", a.span()),
                AstNode::AwaitExpr(a) => ("AwaitExpr", a.span()),
                AstNode::Tpl(a) => ("Tpl", a.span()),
                AstNode::TaggedTpl(a) => ("TaggedTpl", a.span()),
                AstNode::TplElement(a) => ("TplElement", a.span()),
                AstNode::ParenExpr(a) => ("ParenExpr", a.span()),
                AstNode::Super(a) => ("Super", a.span()),
                AstNode::ExprOrSpread(a) => ("ExprOrSpread", a.span()),
                AstNode::OptChainExpr(a) => ("OptChainExpr", a.span()),
                AstNode::Function(a) => ("Function", a.span()),
                AstNode::Param(a) => ("Param", a.span()),
                AstNode::BindingIdent(a) => ("BindingIdent", a.span()),
                AstNode::Ident(a) => ("Ident", a.span()),
                AstNode::PrivateName(a) => ("PrivateName", a.span()),
                AstNode::JSXMemberExpr(a) => ("JSXMemberExpr", a.span()),
                AstNode::JSXNamespacedName(a) => ("JSXNamespacedName", a.span()),
                AstNode::JSXEmptyExpr(a) => ("JSXEmptyExpr", a.span()),
                AstNode::JSXExprContainer(a) => ("JSXExprContainer", a.span()),
                AstNode::JSXSpreadChild(a) => ("JSXSpreadChild", a.span()),
                AstNode::JSXOpeningElement(a) => ("JSXOpeningElement", a.span()),
                AstNode::JSXClosingElement(a) => ("JSXClosingElement", a.span()),
                AstNode::JSXAttr(a) => ("JSXAttr", a.span()),
                AstNode::JSXText(a) => ("JSXText", a.span()),
                AstNode::JSXElement(a) => ("JSXElement", a.span()),
                AstNode::JSXFragment(a) => ("JSXFragment", a.span()),
                AstNode::JSXOpeningFragment(a) => ("JSXOpeningFragment", a.span()),
                AstNode::JSXClosingFragment(a) => ("JSXClosingFragment", a.span()),
                AstNode::Invalid(a) => ("Invalid", a.span()),
                AstNode::Str(a) => ("Str", a.span()),
                AstNode::Bool(a) => ("Bool", a.span()),
                AstNode::Null(a) => ("Null", a.span()),
                AstNode::Number(a) => ("Number", a.span()),
                AstNode::BigInt(a) => ("BigInt", a.span()),
                AstNode::Regex(a) => ("Regex", a.span()),
                AstNode::ExportDefaultExpr(a) => ("ExportDefaultExpr", a.span()),
                AstNode::ExportDecl(a) => ("ExportDecl", a.span()),
                AstNode::ImportDecl(a) => ("ImportDecl", a.span()),
                AstNode::ExportAll(a) => ("ExportAll", a.span()),
                AstNode::NamedExport(a) => ("NamedExport", a.span()),
                AstNode::ExportDefaultDecl(a) => ("ExportDefaultDecl", a.span()),
                AstNode::ImportDefaultSpecifier(a) => ("ImportDefaultSpecifier", a.span()),
                AstNode::ImportStarAsSpecifier(a) => ("ImportStarAsSpecifier", a.span()),
                AstNode::ImportNamedSpecifier(a) => ("ImportNamedSpecifier", a.span()),
                AstNode::ExportNamespaceSpecifier(a) => ("ExportNamespaceSpecifier", a.span()),
                AstNode::ExportDefaultSpecifier(a) => ("ExportDefaultSpecifier", a.span()),
                AstNode::ExportNamedSpecifier(a) => ("ExportNamedSpecifier", a.span()),
                AstNode::Script(a) => ("Script", a.span()),
                AstNode::Module(a) => ("Module", a.span()),
                AstNode::ArrayPat(a) => ("ArrayPat", a.span()),
                AstNode::ObjectPat(a) => ("ObjectPat", a.span()),
                AstNode::AssignPat(a) => ("AssignPat", a.span()),
                AstNode::RestPat(a) => ("RestPat", a.span()),
                AstNode::KeyValuePatProp(a) => ("KeyValuePatProp", a.span()),
                AstNode::AssignPatProp(a) => ("AssignPatProp", a.span()),
                AstNode::KeyValueProp(a) => ("KeyValueProp", a.span()),
                AstNode::AssignProp(a) => ("AssignProp", a.span()),
                AstNode::GetterProp(a) => ("GetterProp", a.span()),
                AstNode::SetterProp(a) => ("SetterProp", a.span()),
                AstNode::MethodProp(a) => ("MethodProp", a.span()),
                AstNode::ComputedPropName(a) => ("ComputedPropName", a.span()),
                AstNode::BlockStmt(a) => ("BlockStmt", a.span()),
                AstNode::ExprStmt(a) => ("ExprStmt", a.span()),
                AstNode::EmptyStmt(a) => ("EmptyStmt", a.span()),
                AstNode::DebuggerStmt(a) => ("DebuggerStmt", a.span()),
                AstNode::WithStmt(a) => ("WithStmt", a.span()),
                AstNode::ReturnStmt(a) => ("ReturnStmt", a.span()),
                AstNode::LabeledStmt(a) => ("LabeledStmt", a.span()),
                AstNode::BreakStmt(a) => ("BreakStmt", a.span()),
                AstNode::ContinueStmt(a) => ("ContinueStmt", a.span()),
                AstNode::IfStmt(a) => ("IfStmt", a.span()),
                AstNode::SwitchStmt(a) => ("SwitchStmt", a.span()),
                AstNode::ThrowStmt(a) => ("ThrowStmt", a.span()),
                AstNode::TryStmt(a) => ("TryStmt", a.span()),
                AstNode::WhileStmt(a) => ("WhileStmt", a.span()),
                AstNode::DoWhileStmt(a) => ("DoWhileStmt", a.span()),
                AstNode::ForStmt(a) => ("ForStmt", a.span()),
                AstNode::ForInStmt(a) => ("ForInStmt", a.span()),
                AstNode::ForOfStmt(a) => ("ForOfStmt", a.span()),
                AstNode::SwitchCase(a) => ("SwitchCase", a.span()),
                AstNode::CatchClause(a) => ("CatchClause", a.span()),
                AstNode::TsTypeAnn(a) => ("TsTypeAnn", a.span()),
                AstNode::TsTypeParamDecl(a) => ("TsTypeParamDecl", a.span()),
                AstNode::TsTypeParam(a) => ("TsTypeParam", a.span()),
                AstNode::TsTypeParamInstantiation(a) => ("TsTypeParamInstantiation", a.span()),
                AstNode::TsParamProp(a) => ("TsParamProp", a.span()),
                AstNode::TsQualifiedName(a) => ("TsQualifiedName", a.span()),
                AstNode::TsCallSignatureDecl(a) => ("TsCallSignatureDecl", a.span()),
                AstNode::TsConstructSignatureDecl(a) => ("TsConstructSignatureDecl", a.span()),
                AstNode::TsPropertySignature(a) => ("TsPropertySignature", a.span()),
                AstNode::TsGetterSignature(a) => ("TsGetterSignature", a.span()),
                AstNode::TsSetterSignature(a) => ("TsSetterSignature", a.span()),
                AstNode::TsMethodSignature(a) => ("TsMethodSignature", a.span()),
                AstNode::TsIndexSignature(a) => ("TsIndexSignature", a.span()),
                AstNode::TsKeywordType(a) => ("TsKeywordType", a.span()),
                AstNode::TsThisType(a) => ("TsThisType", a.span()),
                AstNode::TsFnType(a) => ("TsFnType", a.span()),
                AstNode::TsConstructorType(a) => ("TsConstructorType", a.span()),
                AstNode::TsTypeRef(a) => ("TsTypeRef", a.span()),
                AstNode::TsTypePredicate(a) => ("TsTypePredicate", a.span()),
                AstNode::TsTypeQuery(a) => ("TsTypeQuery", a.span()),
                AstNode::TsImportType(a) => ("TsImportType", a.span()),
                AstNode::TsTypeLit(a) => ("TsTypeLit", a.span()),
                AstNode::TsArrayType(a) => ("TsArrayType", a.span()),
                AstNode::TsTupleType(a) => ("TsTupleType", a.span()),
                AstNode::TsTupleElement(a) => ("TsTupleElement", a.span()),
                AstNode::TsOptionalType(a) => ("TsOptionalType", a.span()),
                AstNode::TsRestType(a) => ("TsRestType", a.span()),
                AstNode::TsUnionType(a) => ("TsUnionType", a.span()),
                AstNode::TsIntersectionType(a) => ("TsIntersectionType", a.span()),
                AstNode::TsConditionalType(a) => ("TsConditionalType", a.span()),
                AstNode::TsInferType(a) => ("TsInferType", a.span()),
                AstNode::TsParenthesizedType(a) => ("TsParenthesizedType", a.span()),
                AstNode::TsTypeOperator(a) => ("TsTypeOperator", a.span()),
                AstNode::TsIndexedAccessType(a) => ("TsIndexedAccessType", a.span()),
                AstNode::TsMappedType(a) => ("TsMappedType", a.span()),
                AstNode::TsLitType(a) => ("TsLitType", a.span()),
                AstNode::TsTplLitType(a) => ("TsTplLitType", a.span()),
                AstNode::TsInterfaceDecl(a) => ("TsInterfaceDecl", a.span()),
                AstNode::TsInterfaceBody(a) => ("TsInterfaceBody", a.span()),
                AstNode::TsExprWithTypeArgs(a) => ("TsExprWithTypeArgs", a.span()),
                AstNode::TsTypeAliasDecl(a) => ("TsTypeAliasDecl", a.span()),
                AstNode::TsEnumDecl(a) => ("TsEnumDecl", a.span()),
                AstNode::TsEnumMember(a) => ("TsEnumMember", a.span()),
                AstNode::TsModuleDecl(a) => ("TsModuleDecl", a.span()),
                AstNode::TsModuleBlock(a) => ("TsModuleBlock", a.span()),
                AstNode::TsNamespaceDecl(a) => ("TsNamespaceDecl", a.span()),
                AstNode::TsImportEqualsDecl(a) => ("TsImportEqualsDecl", a.span()),
                AstNode::TsExternalModuleRef(a) => ("TsExternalModuleRef", a.span()),
                AstNode::TsExportAssignment(a) => ("TsExportAssignment", a.span()),
                AstNode::TsNamespaceExportDecl(a) => ("TsNamespaceExportDecl", a.span()),
                AstNode::TsAsExpr(a) => ("TsAsExpr", a.span()),
                AstNode::TsTypeAssertion(a) => ("TsTypeAssertion", a.span()),
                AstNode::TsNonNullExpr(a) => ("TsNonNullExpr", a.span()),
                AstNode::TsConstAssertion(a) => ("TsConstAssertion", a.span()),
            };
            f.write_fmt(::core::fmt::Arguments::new_v1(
                &["(", ", ", ")"],
                &match (&name, &span) {
                    (arg0, arg1) => [
                        ::core::fmt::ArgumentV1::new(arg0, ::core::fmt::Display::fmt),
                        ::core::fmt::ArgumentV1::new(arg1, ::core::fmt::Debug::fmt),
                    ],
                },
            ))
        }
    }
    impl<'ast> From<&'ast Expr> for AstNode<'ast> {
        fn from(other: &'ast Expr) -> AstNode<'ast> {
            match other {
                Expr::This(e) => AstNode::ThisExpr(e),
                Expr::Array(e) => AstNode::ArrayLit(e),
                Expr::Object(e) => AstNode::ObjectLit(e),
                Expr::Fn(e) => AstNode::FnExpr(e),
                Expr::Unary(e) => AstNode::UnaryExpr(e),
                Expr::Update(e) => AstNode::UpdateExpr(e),
                Expr::Bin(e) => AstNode::BinExpr(e),
                Expr::Assign(e) => AstNode::AssignExpr(e),
                Expr::Member(e) => AstNode::MemberExpr(e),
                Expr::Cond(e) => AstNode::CondExpr(e),
                Expr::Call(e) => AstNode::CallExpr(e),
                Expr::New(e) => AstNode::NewExpr(e),
                Expr::Seq(e) => AstNode::SeqExpr(e),
                Expr::Ident(e) => AstNode::Ident(e),
                Expr::Lit(e) => AstNode::from(e),
                Expr::Tpl(e) => AstNode::Tpl(e),
                Expr::TaggedTpl(e) => AstNode::TaggedTpl(e),
                Expr::Arrow(e) => AstNode::ArrowExpr(e),
                Expr::Class(e) => AstNode::ClassExpr(e),
                Expr::Yield(e) => AstNode::YieldExpr(e),
                Expr::MetaProp(e) => AstNode::MetaPropExpr(e),
                Expr::Await(e) => AstNode::AwaitExpr(e),
                Expr::Paren(e) => AstNode::ParenExpr(e),
                Expr::JSXMember(e) => AstNode::JSXMemberExpr(e),
                Expr::JSXNamespacedName(e) => AstNode::JSXNamespacedName(e),
                Expr::JSXEmpty(e) => AstNode::JSXEmptyExpr(e),
                Expr::JSXElement(e) => AstNode::JSXElement(e),
                Expr::JSXFragment(e) => AstNode::JSXFragment(e),
                Expr::TsTypeAssertion(e) => AstNode::TsTypeAssertion(e),
                Expr::TsConstAssertion(e) => AstNode::TsConstAssertion(e),
                Expr::TsNonNull(e) => AstNode::TsNonNullExpr(e),
                Expr::TsAs(e) => AstNode::TsAsExpr(e),
                Expr::PrivateName(e) => AstNode::PrivateName(e),
                Expr::OptChain(e) => AstNode::OptChainExpr(e),
                Expr::Invalid(e) => AstNode::Invalid(e),
            }
        }
    }
    impl<'ast> From<&'ast Lit> for AstNode<'ast> {
        fn from(other: &'ast Lit) -> AstNode<'ast> {
            match other {
                Lit::Str(l) => AstNode::Str(l),
                Lit::Bool(l) => AstNode::Bool(l),
                Lit::Null(l) => AstNode::Null(l),
                Lit::Num(l) => AstNode::Number(l),
                Lit::BigInt(l) => AstNode::BigInt(l),
                Lit::Regex(l) => AstNode::Regex(l),
                Lit::JSXText(l) => AstNode::JSXText(l),
            }
        }
    }
    impl<'ast> From<&'ast Stmt> for AstNode<'ast> {
        fn from(other: &'ast Stmt) -> AstNode<'ast> {
            match other {
                Stmt::Block(s) => AstNode::BlockStmt(s),
                Stmt::Empty(s) => AstNode::EmptyStmt(s),
                Stmt::Debugger(s) => AstNode::DebuggerStmt(s),
                Stmt::With(s) => AstNode::WithStmt(s),
                Stmt::Return(s) => AstNode::ReturnStmt(s),
                Stmt::Labeled(s) => AstNode::LabeledStmt(s),
                Stmt::Break(s) => AstNode::BreakStmt(s),
                Stmt::Continue(s) => AstNode::ContinueStmt(s),
                Stmt::If(s) => AstNode::IfStmt(s),
                Stmt::Switch(s) => AstNode::SwitchStmt(s),
                Stmt::Throw(s) => AstNode::ThrowStmt(s),
                Stmt::Try(s) => AstNode::TryStmt(s),
                Stmt::While(s) => AstNode::WhileStmt(s),
                Stmt::DoWhile(s) => AstNode::DoWhileStmt(s),
                Stmt::For(s) => AstNode::ForStmt(s),
                Stmt::ForIn(s) => AstNode::ForInStmt(s),
                Stmt::ForOf(s) => AstNode::ForOfStmt(s),
                Stmt::Decl(s) => AstNode::from(s),
                Stmt::Expr(s) => AstNode::ExprStmt(s),
            }
        }
    }
    impl<'ast> From<&'ast Decl> for AstNode<'ast> {
        fn from(other: &'ast Decl) -> AstNode<'ast> {
            match other {
                Decl::Class(d) => AstNode::ClassDecl(d),
                Decl::Fn(d) => AstNode::FnDecl(d),
                Decl::Var(d) => AstNode::VarDecl(d),
                Decl::TsInterface(d) => AstNode::TsInterfaceDecl(d),
                Decl::TsTypeAlias(d) => AstNode::TsTypeAliasDecl(d),
                Decl::TsEnum(d) => AstNode::TsEnumDecl(d),
                Decl::TsModule(d) => AstNode::TsModuleDecl(d),
            }
        }
    }
    impl<'ast> From<&'ast Program> for AstNode<'ast> {
        fn from(other: &'ast Program) -> AstNode<'ast> {
            match other {
                Program::Module(p) => AstNode::Module(p),
                Program::Script(p) => AstNode::Script(p),
            }
        }
    }
}
