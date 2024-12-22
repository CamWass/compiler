use self::expression::BlockStmtOrExpr;

use super::*;
use crate::lexer::TokenContexts;
use atoms::js_word;
use expression::MaybeParen;
use util::AssignProps;

impl<I: Tokens> Parser<'_, I> {
    /// `tsNextTokenCanFollowModifier`
    fn ts_next_token_can_follow_modifier(&mut self) -> PResult<bool> {
        debug_assert!(self.syntax().typescript());

        // Note: TypeScript's implementation is much more complicated because
        // more things are considered modifiers there.
        // This implementation only handles modifiers not handled by @babel/parser
        // itself. And "static". TODO(swc): Would be nice to avoid lookahead.
        // Want a hasLineBreakUpNext() method...
        self.input.bump();
        Ok(!self.input.had_line_break_before_cur()
            && !is!(self, '(')
            && !is!(self, ')')
            && !is!(self, ':')
            && !is!(self, '=')
            && !is!(self, '?'))
    }

    /// Parses a modifier matching one the given modifier names.
    ///
    /// `tsParseModifier`
    pub(super) fn parse_ts_modifier(
        &mut self,
        allowed_modifiers: &[&'static str],
    ) -> PResult<Option<&'static str>> {
        if !self.syntax().typescript() {
            return Ok(None);
        }

        let pos = {
            let modifier = match cur!(self, true)? {
                Token::Word(Word::Ident(w)) => w,
                _ => return Ok(None),
            };

            allowed_modifiers.iter().position(|s| **s == **modifier)
        };

        if let Some(pos) = pos {
            if self.try_parse_ts_bool(|p| p.ts_next_token_can_follow_modifier().map(Some))? {
                return Ok(Some(allowed_modifiers[pos]));
            }
        }

        Ok(None)
    }

    /// `tsIsListTerminator`
    fn is_ts_list_terminator(&mut self, kind: ParsingContext) -> PResult<bool> {
        debug_assert!(self.syntax().typescript());

        Ok(match kind {
            ParsingContext::EnumMembers | ParsingContext::TypeMembers => is!(self, '}'),
            ParsingContext::HeritageClauseElement { .. } => {
                is!(self, '{') || is!(self, "implements") || is!(self, "extends")
            }
            ParsingContext::TupleElementTypes => is!(self, ']'),
            ParsingContext::TypeParametersOrArguments => is!(self, '>'),
        })
    }

    /// `tsParseList`
    fn parse_ts_list<F>(&mut self, kind: ParsingContext, mut parse_element: F) -> PResult<()>
    where
        F: FnMut(&mut Self) -> PResult<()>,
    {
        debug_assert!(self.syntax().typescript());

        while !self.is_ts_list_terminator(kind)? {
            // Skipping "parseListElement" from the TS source since that's just for error
            // handling.
            parse_element(self)?;
        }
        Ok(())
    }

    /// `tsParseDelimitedList`
    fn eat_ts_delimited_list<F>(
        &mut self,
        kind: ParsingContext,
        mut parse_element: F,
    ) -> PResult<()>
    where
        F: FnMut(&mut Self) -> PResult<()>,
    {
        self.eat_ts_delimited_list_inner(kind, |p| {
            parse_element(p)?;
            Ok(())
        })
    }

    /// `tsParseDelimitedList`
    fn eat_ts_delimited_list_inner<F>(
        &mut self,
        kind: ParsingContext,
        mut parse_element: F,
    ) -> PResult<()>
    where
        F: FnMut(&mut Self) -> PResult<()>,
    {
        debug_assert!(self.syntax().typescript());

        loop {
            trace_cur!(self, parse_ts_delimited_list_inner__element);

            if self.is_ts_list_terminator(kind)? {
                break;
            }
            parse_element(self)?;

            if eat!(self, ',') {
                continue;
            }

            if self.is_ts_list_terminator(kind)? {
                break;
            }

            // Recover
            // const enum D {
            //     d = 10
            //     g = 11
            // }
            if kind == ParsingContext::EnumMembers {
                const TOKEN: &Token = &Token::Comma;
                let cur = format!("{:?}", cur!(self, false).ok());
                self.emit_err(self.input.cur_span(), SyntaxError::Expected(TOKEN, cur));
                continue;
            }
            // This will fail with an error about a missing comma
            expect!(self, ',');
        }

        Ok(())
    }

    /// `tsParseEntityName`
    fn parse_ts_entity_name(&mut self, allow_reserved_words: bool) -> PResult<()> {
        debug_assert!(self.syntax().typescript());

        let init = self.parse_ident_name()?;
        // Handle
        //
        // var a: void.x
        //            ^
        if let Ident {
            sym: js_word!("void"),
            ..
        } = init
        {
            let dot_start = self.input.cur_pos();
            let dot_span = span!(self, dot_start);
            self.emit_err(dot_span, SyntaxError::TS1005)
        }
        while eat!(self, '.') {
            let dot_start = self.input.cur_pos();
            if !is!(self, '#') && !is!(self, IdentName) {
                self.emit_err(Span::new(dot_start, dot_start), SyntaxError::TS1003);
                return Ok(());
            }

            if allow_reserved_words {
                self.parse_ident_name()?;
            } else {
                self.parse_ident(false, false)?;
            }
        }

        Ok(())
    }

    /// `tsParseTypeReference`
    fn parse_ts_type_ref(&mut self) -> PResult<()> {
        trace_cur!(self, parse_ts_type_ref);
        debug_assert!(self.syntax().typescript());

        let start = self.input.cur_pos();

        let has_modifier = self.eat_any_ts_modifier()?;

        // Type name:
        self.parse_ts_entity_name(true)?;
        trace_cur!(self, parse_ts_type_ref__type_args);
        // Type parameters:
        if !self.input.had_line_break_before_cur() && is!(self, '<') {
            self.parse_ts_type_args()?;
        }

        if has_modifier {
            self.emit_err(span!(self, start), SyntaxError::TS2369);
        }

        Ok(())
    }

    /// `tsParseThisTypePredicate`
    fn parse_ts_this_type_predicate(&mut self) -> PResult<()> {
        debug_assert!(self.syntax().typescript());

        if eat!(self, "is") {
            self.parse_ts_type_ann(false)?;
        }

        Ok(())
    }

    /// `tsParseThisTypeNode`
    fn parse_ts_this_type_node(&mut self) -> PResult<()> {
        debug_assert!(self.syntax().typescript());

        expect!(self, "this");

        Ok(())
    }

    /// `tsParseImportType`
    fn parse_ts_import_type(&mut self) -> PResult<()> {
        self.assert_and_bump(&tok!("import"));

        expect!(self, '(');

        let lit = self.parse_lit()?;
        if !matches!(lit, Lit::Str(_)) {
            let span = get_span!(self, lit.node_id());
            self.emit_err(span, SyntaxError::TS1141);
        }

        expect!(self, ')');

        // Qualifier:
        if eat!(self, '.') {
            self.parse_ts_entity_name(false).map(Some)?;
        }

        // Type arguments:
        if is!(self, '<') {
            self.parse_ts_type_args().map(Some)?;
        }

        Ok(())
    }

    /// `tsParseTypeQuery`
    fn parse_ts_type_query(&mut self) -> PResult<()> {
        debug_assert!(self.syntax().typescript());

        expect!(self, "typeof");
        // Expression name:
        if is!(self, "import") {
            self.parse_ts_import_type()?;
        } else {
            self.parse_ts_entity_name(true)?;
        }

        Ok(())
    }

    /// `tsParseTypeParameter`
    fn parse_ts_type_param(&mut self) -> PResult<Span> {
        debug_assert!(self.syntax().typescript());

        let start = self.input.cur_pos();

        // Name:
        self.parse_ident_name()?;
        // Constraint:
        self.eat_then_parse_ts_type(&tok!("extends"))?;
        // Default:
        self.eat_then_parse_ts_type(&tok!('='))?;

        Ok(span!(self, start))
    }

    /// `tsParseTypeParameter`
    pub(super) fn eat_ts_type_params(
        &mut self,
        mut op: impl FnMut(&mut Self, Span),
    ) -> PResult<()> {
        self.in_type().parse_with(|p| {
            p.ts_in_no_context(|p| {
                expect!(p, '<');
                p.eat_ts_delimited_list(ParsingContext::TypeParametersOrArguments, |p| {
                    let span = p.parse_ts_type_param()?;
                    op(p, span);
                    Ok(())
                })?;
                expect!(p, '>');

                Ok(())
            })
        })
    }

    /// `tsParseTypeOrTypePredicateAnnotation`
    pub(super) fn parse_ts_type_or_type_predicate_ann(
        &mut self,
        return_token: &'static Token,
    ) -> PResult<()> {
        debug_assert!(self.syntax().typescript());

        self.in_type().parse_with(|p| {
            if !p.input.eat(return_token) {
                let cur = format!("{:?}", cur!(p, false).ok());
                let span = p.input.cur_span();
                syntax_error!(p, span, SyntaxError::Expected(return_token, cur))
            }

            let has_type_pred_asserts = is!(p, "asserts") && peeked_is!(p, IdentRef);
            if has_type_pred_asserts {
                p.assert_and_bump(&tok!("asserts"));
                cur!(p, false)?;
            }

            let has_type_pred_is = is!(p, IdentRef)
                && peeked_is!(p, "is")
                && !p.input.has_linebreak_between_cur_and_peeked();
            let is_type_predicate = has_type_pred_asserts || has_type_pred_is;
            if !is_type_predicate {
                p.parse_ts_type_ann(false)?;
                return Ok(());
            }

            // Type predicate variable:
            p.parse_ident_name()?;
            // Type annotation:
            if has_type_pred_is {
                p.assert_and_bump(&tok!("is"));
                p.parse_ts_type_ann(false)?;
            }

            Ok(())
        })
    }

    /// `tsTryParse`.
    /// `op` should not modify state.
    fn try_parse_ts_bool<F>(&mut self, op: F) -> PResult<bool>
    where
        F: FnOnce(&mut Self) -> PResult<Option<bool>>,
    {
        if !self.syntax().typescript() {
            return Ok(false);
        }
        let prev_emit_err = self.emit_err;

        let Parser {
            emit_err,
            input,
            program_data: _,
            labels: _,
            potential_arrow_start: _,
            trailing_commas_after_rest: _,
            parenthesised_exprs: _,
        } = &*self;

        let old_emit_err = *emit_err;
        let old_input = input.clone();

        self.emit_err = false;
        let res = op(self);

        match res {
            Ok(Some(res)) if res => {
                self.emit_err = prev_emit_err;
                Ok(res)
            }
            _ => {
                self.emit_err = old_emit_err;
                self.input = old_input;

                Ok(false)
            }
        }
    }

    /// `tsTryParse`
    pub(super) fn try_parse_ts<T, F>(&mut self, op: F) -> Option<T>
    where
        F: FnOnce(&mut Self) -> PResult<Option<T>>,
    {
        if !self.syntax().typescript() {
            return None;
        }
        trace_cur!(self, try_parse_ts);
        let prev_emit_err = self.emit_err;

        let Parser {
            emit_err,
            input,
            program_data: _,
            labels,
            potential_arrow_start,
            trailing_commas_after_rest: _,
            parenthesised_exprs: _,
        } = &*self;

        let old_emit_err = *emit_err;
        let old_input = input.clone();
        let prev_labels_len = labels.len();
        let old_potential_arrow_start = potential_arrow_start.clone();

        self.emit_err = false;
        let res = op(self);

        match res {
            Ok(Some(res)) => {
                self.emit_err = prev_emit_err;
                Some(res)
            }
            Ok(None) | Err(..) => {
                self.emit_err = old_emit_err;
                self.input = old_input;
                self.labels.truncate(prev_labels_len);
                self.potential_arrow_start = old_potential_arrow_start;

                None
            }
        }
    }

    pub(super) fn parse_ts_type_ann(&mut self, eat_colon: bool) -> PResult<Span> {
        debug_assert!(self.syntax().typescript());

        self.in_type().parse_with(|p| {
            if eat_colon {
                p.assert_and_bump(&tok!(':'));
            }

            p.parse_ts_type()
        })
    }

    /// `tsEatThenParseType`
    fn eat_then_parse_ts_type(&mut self, token_to_eat: &'static Token) -> PResult<Option<()>> {
        self.in_type().parse_with(|p| {
            if !p.input.eat(token_to_eat) {
                return Ok(None);
            }

            p.parse_ts_type()?;
            Ok(Some(()))
        })
    }

    /// `tsExpectThenParseType`
    fn expect_then_parse_ts_type(
        &mut self,
        token: &'static Token,
        token_str: &'static str,
    ) -> PResult<()> {
        debug_assert!(self.syntax().typescript());

        self.in_type().parse_with(|p| {
            if !p.input.eat(token) {
                let got = format!("{:?}", cur!(p, false).ok());
                syntax_error!(
                    p,
                    p.input.cur_span(),
                    SyntaxError::Unexpected {
                        got,
                        expected: token_str
                    }
                );
            }

            p.parse_ts_type()?;
            Ok(())
        })
    }

    /// `tsNextThenParseType`
    pub(super) fn next_then_parse_ts_type(&mut self) -> PResult<()> {
        debug_assert!(self.syntax().typescript());

        self.in_type().parse_with(|p| {
            p.input.bump();

            p.parse_ts_type()?;
            Ok(())
        })
    }

    /// `tsParseEnumMember`
    fn parse_ts_enum_member(&mut self) -> PResult<()> {
        debug_assert!(self.syntax().typescript());

        let start = self.input.cur_pos();
        // Computed property names are grammar errors in an enum, so accept just string
        // literal or identifier.
        match *cur!(self, true)? {
            Token::Str { .. } => {
                self.parse_lit()?;
            }
            Token::Num { .. } => {
                self.input.bump();
                let span = span!(self, start);

                // Recover from error
                self.emit_err(span, SyntaxError::TS2452);
            }
            Token::LBracket => {
                self.assert_and_bump(&tok!('['));
                let _ = self.parse_expr(&mut AssignProps::Emit)?;

                self.emit_err(span!(self, start), SyntaxError::TS1164);

                expect!(self, ']');
            }
            _ => {
                self.parse_ident_name()?;
            }
        }

        // Init:
        if eat!(self, '=') {
            self.parse_assignment_expr(&mut AssignProps::Emit)?;
        } else if !(is!(self, ',') || is!(self, '}')) {
            let start = self.input.cur_pos();
            self.input.bump();
            store!(self, ',');
            self.emit_err(Span::new(start, start), SyntaxError::TS1005);
        }

        Ok(())
    }

    /// `tsParseEnumDeclaration`
    pub(super) fn parse_ts_enum_decl(&mut self) -> PResult<()> {
        debug_assert!(self.syntax().typescript());

        self.parse_ident_name()?;
        expect!(self, '{');
        self.eat_ts_delimited_list(ParsingContext::EnumMembers, |p| p.parse_ts_enum_member())?;
        expect!(self, '}');

        Ok(())
    }

    /// `tsParseModuleBlock`
    fn parse_ts_module_block(&mut self) -> PResult<()> {
        trace_cur!(self, parse_ts_module_block);

        debug_assert!(self.syntax().typescript());

        expect!(self, '{');
        // Inside of a module block is considered "top-level", meaning it can have
        // imports and exports.
        self.parse_block_body::<ModuleItem>(false, true, Some(&tok!('}')))?;

        Ok(())
    }

    /// `tsParseModuleOrNamespaceDeclaration`
    fn parse_ts_module_or_ns_decl(&mut self) -> PResult<()> {
        debug_assert!(self.syntax().typescript());

        self.parse_ident_name()?;
        // Body:
        if eat!(self, '.') {
            self.parse_ts_module_or_ns_decl()?;
        } else {
            self.parse_ts_module_block()?;
        }

        Ok(())
    }

    /// `tsParseAmbientExternalModuleDeclaration`
    fn parse_ts_ambient_external_module_decl(&mut self) -> PResult<()> {
        debug_assert!(self.syntax().typescript());

        if is!(self, "global") {
            self.parse_ident_name()?;
        } else if matches!(*cur!(self, true)?, Token::Str { .. }) {
            self.parse_lit()?;
        } else {
            unexpected!(self, "global or a string literal");
        };

        if is!(self, '{') {
            self.parse_ts_module_block()?;
        } else {
            expect!(self, ';');
        }

        Ok(())
    }

    pub fn parse_type(&mut self) -> PResult<()> {
        debug_assert!(self.syntax().typescript());

        self.in_type().parse_ts_type()?;
        Ok(())
    }

    /// Be sure to be in a type context before calling self.
    ///
    /// `tsParseType`
    pub(super) fn parse_ts_type(&mut self) -> PResult<Span> {
        trace_cur!(self, parse_ts_type);

        debug_assert!(self.syntax().typescript());

        // Need to set `ctx.in_type` so that we don't parse JSX in a type context.
        debug_assert!(self.ctx().in_type);

        let start = self.input.cur_pos();

        let ty = self.parse_ts_non_conditional_type()?;
        if self.input.had_line_break_before_cur() || !eat!(self, "extends") {
            return Ok(ty);
        }

        // Extends type:
        self.parse_ts_non_conditional_type()?;

        expect!(self, '?');

        // True type:
        self.parse_ts_type()?;

        expect!(self, ':');

        // False type:
        self.parse_ts_type()?;

        Ok(span!(self, start))
    }

    /// `tsParseNonConditionalType`
    fn parse_ts_non_conditional_type(&mut self) -> PResult<Span> {
        trace_cur!(self, parse_ts_non_conditional_type);

        debug_assert!(self.syntax().typescript());

        let start = self.input.cur_pos();

        if self.is_ts_start_of_fn_type()? {
            self.parse_ts_fn_or_constructor_type(true)?;
            return Ok(span!(self, start));
        }
        if (is!(self, "abstract") && peeked_is!(self, "new")) || is!(self, "new") {
            // As in `new () => Date`
            self.parse_ts_fn_or_constructor_type(false)?;
            return Ok(span!(self, start));
        }

        self.parse_ts_union_type_or_higher()?;
        Ok(span!(self, start))
    }

    fn is_ts_start_of_fn_type(&mut self) -> PResult<bool> {
        debug_assert!(self.syntax().typescript());

        if is!(self, '<') {
            return Ok(true);
        }

        Ok(is!(self, '(') && self.ts_look_ahead(|p| p.is_ts_unambiguously_start_of_fn_type())?)
    }

    /// `tsParseTypeAssertion`
    pub(super) fn parse_ts_type_assertion(&mut self) -> PResult<MaybeParen> {
        debug_assert!(self.syntax().typescript());

        // Type annotation:
        // Not actually necessary to set ctx.in_type because we never reach here if JSX
        // plugin is enabled, but need `tsInType` to satisfy the assertion in
        // `tsParseType`.
        self.in_type().parse_with(|p| p.parse_ts_type())?;
        expect!(self, '>');
        self.parse_unary_expr(&mut AssignProps::Emit)
    }

    /// `tsParseHeritageClause`
    pub(super) fn eat_ts_heritage_clause(
        &mut self,
        mut op: impl FnMut(&mut Self, Span),
    ) -> PResult<()> {
        debug_assert!(self.syntax().typescript());

        self.eat_ts_delimited_list(ParsingContext::HeritageClauseElement, |p| {
            let span = p.parse_expr_with_type_args()?;
            op(p, span);
            Ok(())
        })
    }

    /// `tsParseExpressionWithTypeArguments`
    fn parse_expr_with_type_args(&mut self) -> PResult<Span> {
        debug_assert!(self.syntax().typescript());

        let start = self.input.cur_pos();
        // Note: TS uses parseLeftHandSideExpressionOrHigher,
        // then has grammar errors later if it's not an EntityName.

        // Expression:
        self.parse_ts_entity_name(false)?;
        // Type arguments:
        if is!(self, '<') {
            self.parse_ts_type_args()?;
        }

        Ok(span!(self, start))
    }
    /// `tsParseInterfaceDeclaration`
    pub(super) fn parse_ts_interface_decl(&mut self) -> PResult<()> {
        debug_assert!(self.syntax().typescript());

        let id = self.parse_ident_name()?;
        match id.sym {
            js_word!("string")
            | js_word!("null")
            | js_word!("number")
            | js_word!("object")
            | js_word!("any")
            | js_word!("unknown")
            | js_word!("boolean")
            | js_word!("bigint")
            | js_word!("symbol")
            | js_word!("void")
            | js_word!("never")
            | js_word!("intrinsic") => {
                self.emit_err(get_span!(self, id.node_id), SyntaxError::TS2427);
            }
            _ => {}
        }

        self.try_eat_ts_type_params(|_, _| {})?;

        if eat!(self, "extends") {
            self.eat_ts_heritage_clause(|_, _| {})?;
        }

        // Recover from
        //
        //     interface I extends A extends B {}
        if is!(self, "extends") {
            self.emit_err(self.input.cur_span(), SyntaxError::TS1172);

            while !eof!(self) && !is!(self, '{') {
                self.input.bump();
            }
        }

        // Body:
        self.in_type()
            .parse_with(|p| p.parse_ts_object_type_members())?;

        Ok(())
    }

    /// `tsParseTypeAliasDeclaration`
    fn parse_ts_type_alias_decl(&mut self) -> PResult<()> {
        debug_assert!(self.syntax().typescript());

        // Identifier:
        self.parse_ident_name()?;
        // Type parameters:
        self.try_eat_ts_type_params(|_, _| {})?;
        // Type annotation:
        self.expect_then_parse_ts_type(&tok!('='), "=")?;
        expect!(self, ';');
        Ok(())
    }

    // /// `tsParseImportEqualsDeclaration`
    // pub(super) fn parse_ts_import_equals_decl(&mut self) -> PResult<()> {
    //     debug_assert!(self.syntax().typescript());

    //     // Identifier:
    //     self.parse_ident_name()?;
    //     expect!(self, '=');

    //     // Module reference:
    //     self.parse_ts_module_ref()?;
    //     expect!(self, ';');
    //     Ok(())
    // }

    // /// `tsIsExternalModuleReference`
    // fn is_ts_external_module_ref(&mut self) -> PResult<bool> {
    //     debug_assert!(self.syntax().typescript());

    //     Ok(is!(self, "require") && peeked_is!(self, '('))
    // }

    // /// `tsParseModuleReference`
    // fn parse_ts_module_ref(&mut self) -> PResult<()> {
    //     debug_assert!(self.syntax().typescript());

    //     if self.is_ts_external_module_ref()? {
    //         self.parse_ts_external_module_ref().map(From::from)
    //     } else {
    //         self.parse_ts_entity_name(false).map(From::from)
    //     }
    // }

    // /// `tsParseExternalModuleReference`
    // fn parse_ts_external_module_ref(&mut self) -> PResult<()> {
    //     debug_assert!(self.syntax().typescript());

    //     expect!(self, "require");
    //     expect!(self, '(');
    //     match *cur!(self, true)? {
    //         Token::Str { .. } => {}
    //         _ => unexpected!(self, "a string literal"),
    //     }
    //     self.parse_lit()?;
    //     expect!(self, ')');
    //     Ok(())
    // }

    pub(super) fn ts_look_ahead<T, F>(&mut self, op: F) -> PResult<T>
    where
        F: FnOnce(&mut Self) -> PResult<T>,
    {
        debug_assert!(self.syntax().typescript());

        let Parser {
            emit_err,
            input,
            program_data: _,
            labels,
            potential_arrow_start,
            trailing_commas_after_rest: _,
            parenthesised_exprs: _,
        } = &*self;

        let old_emit_err = *emit_err;
        let prev_labels_len = labels.len();
        let old_potential_arrow_start = potential_arrow_start.clone();
        let old_input = input.clone();

        self.emit_err = false;
        let res = op(self);

        self.emit_err = old_emit_err;
        self.input = old_input;
        self.labels.truncate(prev_labels_len);
        self.potential_arrow_start = old_potential_arrow_start;

        res
    }

    /// `tsIsUnambiguouslyStartOfFunctionType`
    fn is_ts_unambiguously_start_of_fn_type(&mut self) -> PResult<bool> {
        debug_assert!(self.syntax().typescript());

        self.assert_and_bump(&tok!('('));
        if is_one_of!(self, ')', "...") {
            // ( )
            // ( ...
            return Ok(true);
        }
        if self.skip_ts_parameter_start()? {
            if is_one_of!(self, ':', ',', '?', '=') {
                // ( xxx :
                // ( xxx ,
                // ( xxx ?
                // ( xxx =
                return Ok(true);
            }
            if eat!(self, ')') && is!(self, "=>") {
                // ( xxx ) =>
                return Ok(true);
            }
        }
        Ok(false)
    }

    /// `tsSkipParameterStart`
    fn skip_ts_parameter_start(&mut self) -> PResult<bool> {
        debug_assert!(self.syntax().typescript());

        let _ = self.eat_any_ts_modifier()?;

        if is_one_of!(self, IdentRef, "this") {
            self.input.bump();
            return Ok(true);
        }

        if is!(self, '{') {
            let mut brace_stack_counter = 1;
            self.input.bump();

            while brace_stack_counter > 0 {
                if is!(self, '{') {
                    brace_stack_counter += 1;
                } else if is!(self, '}') {
                    brace_stack_counter -= 1;
                }
                self.input.bump();
            }
            return Ok(true);
        }

        if is!(self, '[') {
            let mut bracket_stack_counter = 1;
            self.input.bump();

            while bracket_stack_counter > 0 {
                if is!(self, '[') {
                    bracket_stack_counter += 1;
                } else if is!(self, ']') {
                    bracket_stack_counter -= 1;
                }
                self.input.bump();
            }
            return Ok(true);
        }

        Ok(false)
    }

    /// `tsParseTypeMemberSemicolon`
    fn parse_ts_type_member_semicolon(&mut self) -> PResult<()> {
        debug_assert!(self.syntax().typescript());

        if !eat!(self, ',') {
            expect!(self, ';');
        }

        Ok(())
    }

    /// `tsParseSignatureMember`
    fn parse_ts_signature_member(&mut self, kind: SignatureParsingMode) -> PResult<()> {
        debug_assert!(self.syntax().typescript());

        if kind == SignatureParsingMode::TSConstructSignatureDeclaration {
            expect!(self, "new");
        }

        // ----- inlined self.tsFillSignature(tt.colon, node);
        // Type parameters:
        self.try_eat_ts_type_params(|_, _| {})?;
        expect!(self, '(');
        // Params:
        self.parse_ts_binding_list_for_signature()?;
        // Type annotation:
        if is!(self, ':') {
            self.parse_ts_type_or_type_predicate_ann(&tok!(':'))?;
        }
        // -----

        self.parse_ts_type_member_semicolon()?;

        Ok(())
    }

    /// `tsIsUnambiguouslyIndexSignature`
    fn is_ts_unambiguously_index_signature(&mut self) -> PResult<bool> {
        debug_assert!(self.syntax().typescript());

        // Note: babel's comment is wrong
        self.assert_and_bump(&tok!('[')); // Skip '['

        // ',' is for error recovery
        Ok(eat!(self, IdentRef) && is_one_of!(self, ':', ','))
    }

    /// `tsTryParseIndexSignature`
    pub(super) fn try_parse_ts_index_signature(&mut self) -> PResult<Option<()>> {
        if !(is!(self, '[') && self.ts_look_ahead(|p| p.is_ts_unambiguously_index_signature())?) {
            return Ok(None);
        }

        expect!(self, '[');

        let ident_start = self.input.cur_pos();
        let id = self
            .parse_ident_name()
            .map(|i| BindingIdent::from_ident(i, program_data!(self)))?;

        if eat!(self, ',') {
            self.emit_err(get_span!(self, id.id.node_id), SyntaxError::TS1096);
        } else {
            expect!(self, ':');
        }

        // Type annotation:
        self.parse_ts_type_ann(false)?;
        set_span!(self, id.id.node_id, span!(self, ident_start));

        expect!(self, ']');

        // Type annotation:
        self.try_parse_ts_type_ann()?;

        self.parse_ts_type_member_semicolon()?;
        Ok(Some(()))
    }

    /// `tsParsePropertyOrMethodSignature`
    fn parse_ts_property_or_method_signature(&mut self, readonly: bool) -> PResult<()> {
        debug_assert!(self.syntax().typescript());

        // Key:
        self.parse_prop_name()?;

        eat!(self, '?');

        if !readonly && is_one_of!(self, '(', '<') {
            // ----- inlined self.tsFillSignature(tt.colon, node);
            // Type parameters:
            self.try_eat_ts_type_params(|_, _| {})?;
            expect!(self, '(');
            // Parameters:
            self.parse_ts_binding_list_for_signature()?;
            // Type annotation:
            if is!(self, ':') {
                self.parse_ts_type_or_type_predicate_ann(&tok!(':'))?;
            }
            // -----

            self.parse_ts_type_member_semicolon()?;
            Ok(())
        } else {
            // Type annotation:
            self.try_parse_ts_type_ann()?;

            self.parse_ts_type_member_semicolon()?;
            Ok(())
        }
    }

    /// `tsParseTypeMember`
    fn parse_ts_type_member(&mut self) -> PResult<()> {
        debug_assert!(self.syntax().typescript());

        if is_one_of!(self, '(', '<') {
            return self
                .parse_ts_signature_member(SignatureParsingMode::TSCallSignatureDeclaration);
        }
        if is!(self, "new") && self.ts_look_ahead(|p| p.is_ts_start_of_construct_signature())? {
            return self
                .parse_ts_signature_member(SignatureParsingMode::TSConstructSignatureDeclaration);
        }
        let readonly = self.parse_ts_modifier(&["readonly"])?.is_some();

        let idx = self.try_parse_ts_index_signature()?;
        if idx.is_some() {
            return Ok(());
        }

        if let Some(v) = self.try_parse_ts(|p| {
            let _ = p.parse_ts_modifier(&["readonly"])?.is_some();

            let is_get = if eat!(p, "get") {
                true
            } else {
                expect!(p, "set");
                false
            };

            // Key:
            p.parse_prop_name()?;

            eat!(p, '?');

            if is_get {
                expect!(p, '(');
                expect!(p, ')');
                // Type annotation:
                p.try_parse_ts_type_ann()?;

                p.parse_ts_type_member_semicolon()?;

                Ok(Some(()))
            } else {
                expect!(p, '(');
                let params = p.parse_ts_binding_list_for_signature()?;
                if params == 0 {
                    syntax_error!(p, SyntaxError::SetterParamRequired)
                }

                p.parse_ts_type_member_semicolon()?;

                Ok(Some(()))
            }
        }) {
            return Ok(v);
        }

        self.parse_ts_property_or_method_signature(readonly)
    }

    /// `tsIsStartOfConstructSignature`
    fn is_ts_start_of_construct_signature(&mut self) -> PResult<bool> {
        debug_assert!(self.syntax().typescript());

        self.input.bump();

        Ok(is!(self, '(') || is!(self, '<'))
    }

    /// `tsParseTypeLiteral`
    fn parse_ts_type_lit(&mut self) -> PResult<()> {
        debug_assert!(self.syntax().typescript());

        self.parse_ts_object_type_members()?;
        Ok(())
    }

    /// `tsParseObjectTypeMembers`
    fn parse_ts_object_type_members(&mut self) -> PResult<()> {
        debug_assert!(self.syntax().typescript());

        expect!(self, '{');
        self.parse_ts_list(ParsingContext::TypeMembers, |p| p.parse_ts_type_member())?;
        expect!(self, '}');
        Ok(())
    }

    /// `tsIsStartOfMappedType`
    fn is_ts_start_of_mapped_type(&mut self) -> PResult<bool> {
        debug_assert!(self.syntax().typescript());

        self.input.bump();
        if eat!(self, '+') || eat!(self, '-') {
            return Ok(is!(self, "readonly"));
        }
        if is!(self, "readonly") {
            self.input.bump();
        }
        if !is!(self, '[') {
            return Ok(false);
        }
        self.input.bump();
        if !is!(self, IdentRef) {
            return Ok(false);
        }
        self.input.bump();

        Ok(is!(self, "in"))
    }

    /// `tsParseMappedTypeParameter`
    fn parse_ts_mapped_type_param(&mut self) -> PResult<()> {
        debug_assert!(self.syntax().typescript());

        // Name:
        self.parse_ident_name()?;
        // Constraint
        self.expect_then_parse_ts_type(&tok!("in"), "in")?;

        Ok(())
    }

    /// `tsParseMappedType`
    fn parse_ts_mapped_type(&mut self) -> PResult<()> {
        debug_assert!(self.syntax().typescript());

        expect!(self, '{');
        if is_one_of!(self, '+', '-') {
            self.input.bump();
            expect!(self, "readonly");
        } else {
            eat!(self, "readonly");
        }

        expect!(self, '[');
        // Type parameter:
        self.parse_ts_mapped_type_param()?;
        // Name type:
        if eat!(self, "as") {
            self.parse_ts_type()?;
        }
        expect!(self, ']');

        if is_one_of!(self, '+', '-') {
            self.input.bump();
            expect!(self, '?');
        } else {
            eat!(self, '?');
        }

        self.try_parse_ts_type()?;
        expect!(self, ';');
        expect!(self, '}');

        Ok(())
    }

    /// `tsParseTupleType`
    fn parse_ts_tuple_type(&mut self) -> PResult<()> {
        debug_assert!(self.syntax().typescript());

        expect!(self, '[');

        // Validate the elementTypes to ensure:
        //   No mandatory elements may follow optional elements
        //   If there's a rest element, it must be at the end of the tuple
        let mut seen_optional_element = false;
        self.eat_ts_delimited_list(ParsingContext::TupleElementTypes, |p| {
            let start = p.input.cur_pos();
            let kind = p.parse_ts_tuple_element_type()?;
            match kind {
                TupleElementType::Rest => {}
                TupleElementType::Optional => {
                    seen_optional_element = true;
                }
                TupleElementType::Other if seen_optional_element => {
                    syntax_error!(p, span!(p, start), SyntaxError::TsRequiredAfterOptional)
                }
                _ => {}
            }
            Ok(())
        })?;

        expect!(self, ']');

        Ok(())
    }

    fn try_parse_ts_tuple_element_name(&mut self) -> Option<Pat> {
        self.try_parse_ts(|p| {
            let start = p.input.cur_pos();

            let rest = eat!(p, "...");

            let ident = p.parse_ident_name()?;
            if eat!(p, '?') {
                let s = get_span!(p, ident.node_id).with_hi(p.input.prev_span().hi);
                set_span!(p, ident.node_id, s);
            }
            expect!(p, ':');

            Ok(Some(if rest {
                Pat::Rest(RestPat {
                    node_id: node_id!(p, span!(p, start)),
                    arg: Box::new(Pat::Ident(BindingIdent::from_ident(
                        ident,
                        program_data!(p),
                    ))),
                })
            } else {
                Pat::Ident(BindingIdent::from_ident(ident, program_data!(p)))
            }))
        })
    }

    /// `tsParseTupleElementType`
    fn parse_ts_tuple_element_type(&mut self) -> PResult<TupleElementType> {
        debug_assert!(self.syntax().typescript());

        // parses `...TsType[]`

        // Label:
        self.try_parse_ts_tuple_element_name();

        if eat!(self, "...") {
            // Type annotation:
            self.parse_ts_type()?;
            return Ok(TupleElementType::Rest);
        }

        self.parse_ts_type()?;
        // parses `TsType?`
        if eat!(self, '?') {
            return Ok(TupleElementType::Optional);
        }

        Ok(TupleElementType::Other)
    }

    /// `tsParseParenthesizedType`
    fn parse_ts_parenthesized_type(&mut self) -> PResult<()> {
        debug_assert!(self.syntax().typescript());

        expect!(self, '(');
        self.parse_ts_type()?;
        expect!(self, ')');
        Ok(())
    }

    /// `tsParseFunctionOrConstructorType`
    fn parse_ts_fn_or_constructor_type(&mut self, is_fn_type: bool) -> PResult<()> {
        trace_cur!(self, parse_ts_fn_or_constructor_type);

        debug_assert!(self.syntax().typescript());

        if !is_fn_type {
            eat!(self, "abstract");
            expect!(self, "new");
        }

        // ----- inlined `self.tsFillSignature(tt.arrow, node)`
        // Type parameters:
        self.try_eat_ts_type_params(|_, _| {})?;
        expect!(self, '(');
        // Parameters:
        self.parse_ts_binding_list_for_signature()?;
        // Type annotation:
        self.parse_ts_type_or_type_predicate_ann(&tok!("=>"))?;
        // ----- end

        Ok(())
    }

    /// `tsParseLiteralTypeNode`
    fn parse_ts_lit_type_node(&mut self) -> PResult<()> {
        debug_assert!(self.syntax().typescript());

        if is!(self, '`') {
            self.parse_ts_tpl_lit_type()?;
        } else {
            self.parse_lit()?;
        }

        Ok(())
    }

    /// `tsParseTemplateLiteralType`
    fn parse_ts_tpl_lit_type(&mut self) -> PResult<()> {
        debug_assert!(self.syntax().typescript());

        self.assert_and_bump(&tok!('`'));

        self.parse_ts_tpl_type_elements()?;

        expect!(self, '`');

        Ok(())
    }

    fn parse_ts_tpl_type_elements(&mut self) -> PResult<()> {
        trace_cur!(self, parse_tpl_elements);

        self.parse_tpl_element(false)?;

        while !is!(self, '`') {
            expect!(self, "${");
            self.parse_ts_type()?;
            expect!(self, '}');
            self.parse_tpl_element(false)?;
        }

        Ok(())
    }

    /// `tsParseBindingListForSignature`
    ///
    /// Eats ')` at the end but does not eat `(` at start.
    fn parse_ts_binding_list_for_signature(&mut self) -> PResult<usize> {
        debug_assert!(self.syntax().typescript());

        let params = self.parse_formal_params()?;

        let mut count: usize = 0;

        for param in params {
            match param.pat {
                Pat::Ident(_) | Pat::Array(_) | Pat::Object(_) | Pat::Rest(_) => {
                    count += 1;
                }
                _ => unexpected!(
                    self,
                    "an identifier, [ for an array pattern, { for an object patter or ... for a \
                     rest pattern"
                ),
            };
        }
        expect!(self, ')');
        Ok(count)
    }

    /// `tsTryParseTypeOrTypePredicateAnnotation`
    ///
    /// Used for parsing return types.
    fn try_parse_ts_type_or_type_predicate_ann(&mut self) -> PResult<Option<()>> {
        if is!(self, ':') {
            self.parse_ts_type_or_type_predicate_ann(&tok!(':'))
                .map(Some)
        } else {
            Ok(None)
        }
    }

    /// `tsTryParseTypeAnnotation`
    pub(super) fn try_parse_ts_type_ann(&mut self) -> PResult<Option<Span>> {
        if is!(self, ':') {
            return self.parse_ts_type_ann(true).map(Some);
        }

        Ok(None)
    }

    /// `tsTryParseType`
    fn try_parse_ts_type(&mut self) -> PResult<Option<()>> {
        self.eat_then_parse_ts_type(&tok!(':'))
    }

    /// `tsTryParseTypeParameters`
    pub(super) fn try_eat_ts_type_params(
        &mut self,
        mut op: impl FnMut(&mut Self, Span),
    ) -> PResult<()> {
        if is!(self, '<') {
            self.eat_ts_type_params(|p, span| op(p, span))?;
        }
        Ok(())
    }

    /// `tsParseNonArrayType`
    fn parse_ts_non_array_type(&mut self) -> PResult<()> {
        trace_cur!(self, parse_ts_non_array_type);
        debug_assert!(self.syntax().typescript());

        match *cur!(self, true)? {
            Token::Word(Word::Ident(..))
            | tok!("void")
            | tok!("yield")
            | tok!("null")
            | tok!("await")
            | tok!("break") => {
                if is!(self, "asserts") && peeked_is!(self, "this") {
                    self.input.bump();
                    self.parse_ts_this_type_node()?;
                    return self.parse_ts_this_type_predicate();
                }

                let valid_kind = is_one_of!(
                    self,
                    "void",
                    "null",
                    "any",
                    "boolean",
                    "bigint",
                    "never",
                    "number",
                    "object",
                    "string",
                    "symbol",
                    "unknown",
                    "undefined",
                    "intrinsic"
                );

                let peeked_is_dot = peeked_is!(self, '.');

                if valid_kind && !peeked_is_dot {
                    self.input.bump();
                    return Ok(());
                } else {
                    return self.parse_ts_type_ref();
                }
            }
            Token::BigInt { .. }
            | Token::Str { .. }
            | Token::Num { .. }
            | tok!("true")
            | tok!("false")
            | tok!('`') => {
                return self.parse_ts_lit_type_node();
            }
            tok!('-') => {
                self.input.bump();

                if !matches!(*cur!(self, true)?, Token::Num { .. }) {
                    unexpected!(self, "a numeric literal")
                }

                self.parse_lit()?;

                return Ok(());
            }

            tok!("import") => {
                return self.parse_ts_import_type();
            }

            tok!("this") => {
                // This keyword:
                self.parse_ts_this_type_node()?;
                if !self.input.had_line_break_before_cur() && is!(self, "is") {
                    return self.parse_ts_this_type_predicate();
                } else {
                    return Ok(());
                }
            }
            tok!("typeof") => {
                return self.parse_ts_type_query();
            }

            tok!('{') => {
                return if self.ts_look_ahead(|p| p.is_ts_start_of_mapped_type())? {
                    self.parse_ts_mapped_type()
                } else {
                    self.parse_ts_type_lit()
                };
            }
            tok!('[') => {
                return self.parse_ts_tuple_type();
            }
            tok!('(') => {
                return self.parse_ts_parenthesized_type();
            }
            _ => {}
        }

        unexpected!(
            self,
            "an identifier, void, yield, null, await, break, a string literal, a numeric literal, \
             true, false, `, -, import, this, typeof, {, [, ("
        )
    }

    /// `tsParseArrayTypeOrHigher`
    fn parse_ts_array_type_or_higher(&mut self) -> PResult<()> {
        trace_cur!(self, parse_ts_array_type_or_higher);
        debug_assert!(self.syntax().typescript());

        self.parse_ts_non_array_type()?;

        while !self.input.had_line_break_before_cur() && eat!(self, '[') {
            if eat!(self, ']') {
            } else {
                // Index type:
                self.parse_ts_type()?;
                expect!(self, ']');
            }
        }

        Ok(())
    }

    /// `tsParseTypeOperator`
    fn parse_ts_type_operator(&mut self, op: TsTypeOperatorOp) -> PResult<()> {
        debug_assert!(self.syntax().typescript());

        match op {
            TsTypeOperatorOp::Unique => expect!(self, "unique"),
            TsTypeOperatorOp::KeyOf => expect!(self, "keyof"),
            TsTypeOperatorOp::ReadOnly => expect!(self, "readonly"),
        }

        // Type annotation:
        self.parse_ts_type_operator_or_higher()?;
        Ok(())
    }

    /// `tsParseInferType`
    fn parse_ts_infer_type(&mut self) -> PResult<()> {
        debug_assert!(self.syntax().typescript());

        expect!(self, "infer");
        self.parse_ident_name()?;

        Ok(())
    }

    /// `tsParseTypeOperatorOrHigher`
    fn parse_ts_type_operator_or_higher(&mut self) -> PResult<()> {
        trace_cur!(self, parse_ts_type_operator_or_higher);
        debug_assert!(self.syntax().typescript());

        let operator = if is!(self, "keyof") {
            Some(TsTypeOperatorOp::KeyOf)
        } else if is!(self, "unique") {
            Some(TsTypeOperatorOp::Unique)
        } else if is!(self, "readonly") {
            Some(TsTypeOperatorOp::ReadOnly)
        } else {
            None
        };

        match operator {
            Some(operator) => self.parse_ts_type_operator(operator),
            None => {
                trace_cur!(self, parse_ts_type_operator_or_higher__not_operator);

                if is!(self, "infer") {
                    self.parse_ts_infer_type()
                } else {
                    self.parse_ts_modifier(&["readonly"])?;
                    self.parse_ts_array_type_or_higher()
                }
            }
        }
    }

    /// `tsParseExpressionStatement`
    pub(super) fn parse_ts_expr_stmt(&mut self, expr: &Ident) -> PResult<Option<DeclOrEmpty>> {
        let start = get_span!(self, expr.node_id).lo();

        match &*expr.sym {
            "declare" => self.try_parse_ts_declare(start),
            "global" => {
                // `global { }` (with no `declare`) may appear inside an ambient module
                // declaration.
                // Would like to use tsParseAmbientExternalModuleDeclaration here, but already
                // ran past "global".
                if is!(self, '{') {
                    // Body:
                    self.parse_ts_module_block()?;
                    Ok(None)
                } else {
                    Ok(None)
                }
            }
            _ => self.parse_ts_decl(start, expr.sym.clone(), false),
        }
    }

    /// `tsTryParseDeclare`
    pub(super) fn try_parse_ts_declare(&mut self, start: BytePos) -> PResult<Option<DeclOrEmpty>> {
        assert!(
            !is!(self, "declare"),
            "try_parse_ts_declare should be called after eating `declare`"
        );

        if self.ctx().in_declare {
            let span_of_declare = span!(self, start);
            self.emit_err(span_of_declare, SyntaxError::TS1038);
        }

        let declare_start = start;
        let ctx = Context {
            in_declare: true,
            ..self.ctx()
        };

        self.with_ctx(ctx).parse_with(|p| {
            if is!(p, "function") {
                let decl = p.parse_fn_decl_or_ts_overload_sig()?;
                if let Some(Decl::Fn(f)) = &decl {
                    let mut s = get_span!(p, f.function.node_id);
                    s.lo = declare_start;
                    set_span!(p, f.node_id, s);
                }
                return Ok(decl.map(DeclOrEmpty::Decl));
            }

            if is!(p, "class") {
                let decl = p.parse_class_decl(start, start)?;
                // Should always be the case.
                if let Decl::Class(c) = &decl {
                    let mut s = get_span!(p, c.class.node_id);
                    s.lo = declare_start;
                    set_span!(p, c.node_id, s);
                }
                return Ok(Some(DeclOrEmpty::Decl(decl)));
            }

            if is!(p, "const") && peeked_is!(p, "enum") {
                p.assert_and_bump(&tok!("const"));
                let _ = cur!(p, true);
                p.assert_and_bump(&tok!("enum"));

                p.parse_ts_enum_decl()?;
                return Ok(Some(DeclOrEmpty::Empty));
            }
            if is_one_of!(p, "const", "var", "let") {
                let decl = p.parse_var_stmt(false)?;
                let mut s = get_span!(p, decl.node_id);
                s.lo = declare_start;
                set_span!(p, decl.node_id, s);
                return Ok(Some(DeclOrEmpty::Decl(Decl::Var(decl))));
            }

            if is!(p, "global") {
                p.parse_ts_ambient_external_module_decl()?;
            } else if is!(p, IdentName) {
                let value = match cur!(p, true)? {
                    Token::Word(w) => w.clone().into(),
                    _ => unreachable!(),
                };
                return p.parse_ts_decl(start, value, true);
            }

            Ok(None)
        })
    }

    /// `tsTryParseExportDeclaration`
    ///
    /// Note: this won't be called unless the keyword is allowed in
    /// `shouldParseExportDeclaration`.
    pub(super) fn try_parse_ts_export_decl(&mut self, value: JsWord) -> Option<DeclOrEmpty> {
        self.try_parse_ts(|p| {
            let start = p.input.cur_pos();
            let opt = p.parse_ts_decl(start, value, true)?;
            Ok(opt)
        })
    }

    /// Common to tsTryParseDeclare, tsTryParseExportDeclaration, and
    /// tsParseExpressionStatement.
    ///
    /// `tsParseDeclaration`
    fn parse_ts_decl(
        &mut self,
        start: BytePos,
        value: JsWord,
        next: bool,
    ) -> PResult<Option<DeclOrEmpty>> {
        match value {
            js_word!("abstract") => {
                if next || (is!(self, "class") && !self.input.had_line_break_before_cur()) {
                    if next {
                        self.input.bump();
                    }
                    let decl = self.parse_class_decl(start, start)?;
                    return Ok(Some(DeclOrEmpty::Decl(decl)));
                }
            }

            js_word!("enum") => {
                if next || is!(self, IdentRef) {
                    if next {
                        self.input.bump();
                    }
                    self.parse_ts_enum_decl()?;
                    return Ok(Some(DeclOrEmpty::Empty));
                }
            }

            js_word!("interface") => {
                if next || (is!(self, IdentRef)) {
                    if next {
                        self.input.bump();
                    }
                    self.parse_ts_interface_decl()?;
                    return Ok(Some(DeclOrEmpty::Empty));
                }
            }

            js_word!("module") => {
                if next {
                    self.input.bump();
                }

                if matches!(*cur!(self, true)?, Token::Str { .. }) {
                    self.parse_ts_ambient_external_module_decl()?;
                    return Ok(Some(DeclOrEmpty::Empty));
                } else if next || is!(self, IdentRef) {
                    self.parse_ts_module_or_ns_decl()?;
                    return Ok(Some(DeclOrEmpty::Empty));
                }
            }

            js_word!("namespace") => {
                if next || is!(self, IdentRef) {
                    if next {
                        self.input.bump();
                    }
                    self.parse_ts_module_or_ns_decl()?;
                    return Ok(Some(DeclOrEmpty::Empty));
                }
            }

            js_word!("type") => {
                if next || is!(self, IdentRef) {
                    if next {
                        self.input.bump();
                    }
                    self.parse_ts_type_alias_decl()?;
                    return Ok(Some(DeclOrEmpty::Empty));
                }
            }

            _ => {}
        }

        Ok(None)
    }

    /// `tsTryParseGenericAsyncArrowFunction`
    pub(super) fn try_parse_ts_generic_async_arrow_fn(
        &mut self,
        start: BytePos,
    ) -> PResult<Option<ArrowExpr>> {
        let res = if is!(self, '<') {
            self.try_parse_ts(|p| {
                // Type parameters:
                p.eat_ts_type_params(|_, _| {})?;
                // Don't use overloaded parseFunctionParams which would look for "<" again.
                expect!(p, '(');
                let params = p.parse_formal_params()?;
                expect!(p, ')');
                // Return type:
                p.try_parse_ts_type_or_type_predicate_ann()?;
                expect!(p, "=>");

                Ok(Some(params))
            })
        } else {
            None
        };

        let params = match res {
            Some(v) => v,
            None => return Ok(None),
        };

        let ctx = Context {
            in_async: true,
            in_generator: false,
            ..self.ctx()
        };
        self.with_ctx(ctx).parse_with(|p| {
            let is_async = true;
            let body: BlockStmtOrExpr = p.parse_fn_body(true, false)?;
            let body = p.make_arrow_fn_block(body);
            Ok(Some(ArrowExpr {
                node_id: node_id!(p, span!(p, start)),
                body,
                is_async,
                params,
            }))
        })
    }

    /// `tsParseTypeArguments`
    pub fn parse_ts_type_args(&mut self) -> PResult<()> {
        trace_cur!(self, parse_ts_type_args);
        debug_assert!(self.syntax().typescript());

        // Params
        self.in_type().parse_with(|p| {
            // Temporarily remove a JSX parsing context, which makes us scan different
            // tokens.
            p.ts_in_no_context(|p| {
                expect!(p, '<');
                p.eat_ts_delimited_list(ParsingContext::TypeParametersOrArguments, |p| {
                    trace_cur!(p, parse_ts_type_args__arg);

                    p.parse_ts_type()?;
                    Ok(())
                })
            })
        })?;
        // This reads the next token after the `>` too, so do this in the enclosing
        // context. But be sure not to parse a regex in the jsx expression
        // `<C<number> />`, so set exprAllowed = false
        self.input.set_expr_allowed(false);
        expect!(self, '>');
        Ok(())
    }

    /// `tsParseIntersectionTypeOrHigher`
    fn parse_ts_intersection_type_or_higher(&mut self) -> PResult<()> {
        trace_cur!(self, parse_ts_intersection_type_or_higher);

        debug_assert!(self.syntax().typescript());

        self.parse_ts_union_or_intersection_type(
            |p| p.parse_ts_type_operator_or_higher(),
            &tok!('&'),
        )
    }

    /// `tsParseUnionTypeOrHigher`
    fn parse_ts_union_type_or_higher(&mut self) -> PResult<()> {
        trace_cur!(self, parse_ts_union_type_or_higher);
        debug_assert!(self.syntax().typescript());

        self.parse_ts_union_or_intersection_type(
            |p| p.parse_ts_intersection_type_or_higher(),
            &tok!('|'),
        )
    }

    /// `tsParseUnionOrIntersectionType`
    fn parse_ts_union_or_intersection_type<F>(
        &mut self,
        mut parse_constituent_type: F,
        operator: &'static Token,
    ) -> PResult<()>
    where
        F: FnMut(&mut Self) -> PResult<()>,
    {
        trace_cur!(self, parse_ts_union_or_intersection_type);

        debug_assert!(self.syntax().typescript());

        self.input.eat(operator);
        trace_cur!(self, parse_ts_union_or_intersection_type__first_type);

        parse_constituent_type(self)?;
        trace_cur!(self, parse_ts_union_or_intersection_type__after_first);

        if self.input.is(operator) {
            while self.input.eat(operator) {
                trace_cur!(self, parse_ts_union_or_intersection_type__constituent);

                parse_constituent_type(self)?;
            }

            return Ok(());
        }

        Ok(())
    }
}

impl<I: Tokens> Parser<'_, I> {
    /// In no lexer context
    fn ts_in_no_context<T, F>(&mut self, op: F) -> PResult<T>
    where
        F: FnOnce(&mut Self) -> PResult<T>,
    {
        debug_assert!(self.syntax().typescript());

        let cloned = self.input.token_context().clone();
        self.input
            .set_token_context(TokenContexts(vec![cloned.0[0]]));
        let res = op(self);
        self.input.set_token_context(cloned);

        res
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ParsingContext {
    EnumMembers,
    HeritageClauseElement,
    TupleElementTypes,
    TypeMembers,
    TypeParametersOrArguments,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum SignatureParsingMode {
    TSCallSignatureDeclaration,
    TSConstructSignatureDeclaration,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum TsTypeOperatorOp {
    /// `keyof`
    KeyOf,
    /// `unique`
    Unique,
    /// `readonly`
    ReadOnly,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum TupleElementType {
    Rest,
    Optional,
    Other,
}

pub enum DeclOrEmpty {
    Decl(Decl),
    Empty,
}
