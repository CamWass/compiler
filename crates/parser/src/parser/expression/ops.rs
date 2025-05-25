//! Parser for unary operations and binary operations.
use util::AssignProps;

use super::*;
use crate::token::Keyword;

impl<I: Tokens> Parser<'_, I> {
    /// Name from spec: 'LogicalORExpression'
    pub(super) fn parse_bin_expr(&mut self, assign_props: &mut AssignProps) -> PResult<MaybeParen> {
        trace_cur!(self, parse_bin_expr);

        let include_in_expr = self.ctx().include_in_expr();

        let potential_arrow_start = self.potential_arrow_start;

        let left = match self.parse_unary_expr(assign_props) {
            Ok(v) => v,
            Err(err) => {
                trace_cur!(self, parse_bin_expr__recovery_unary_err);

                match cur!(self, true)? {
                    &Word(Word::Keyword(Keyword::In)) if include_in_expr => {
                        self.emit_err(self.input.cur_span(), SyntaxError::TS1109);

                        Box::new(Expr::Invalid(Invalid {
                            node_id: node_id!(self, err.error.0),
                        }))
                        .into()
                    }
                    &Word(Word::Keyword(Keyword::InstanceOf)) | &Token::BinOp(..) => {
                        self.emit_err(self.input.cur_span(), SyntaxError::TS1109);

                        Box::new(Expr::Invalid(Invalid {
                            node_id: node_id!(self, err.error.0),
                        }))
                        .into()
                    }
                    _ => return Err(err),
                }
            }
        };

        return_if_arrow!(self, potential_arrow_start, left);
        self.parse_bin_op_recursively(left, 0)
    }

    /// Parse binary operators with the operator precedence parsing
    /// algorithm. `left` is the left-hand side of the operator.
    /// `minPrec` provides context that allows the function to stop and
    /// defer further parser to one of its callers when it encounters an
    /// operator that has a lower precedence than the set it is parsing.
    ///
    /// `parseExprOp`
    pub(in crate::parser) fn parse_bin_op_recursively(
        &mut self,
        mut left: MaybeParen,
        mut min_prec: u8,
    ) -> PResult<MaybeParen> {
        loop {
            let (next_left, next_prec) = self.parse_bin_op_recursively_inner(left, min_prec)?;

            if let MaybeParen::Expr(next_left) = &next_left {
                match next_left.as_ref() {
                    Expr::Bin(BinExpr {
                        node_id,
                        left,
                        op: op!("&&"),
                        ..
                    })
                    | Expr::Bin(BinExpr {
                        node_id,
                        left,
                        op: op!("||"),
                        ..
                    }) => {
                        if let Expr::Bin(BinExpr { op: op!("??"), .. }) = &**left {
                            self.emit_err(
                                get_span!(self, *node_id),
                                SyntaxError::NullishCoalescingWithLogicalOp,
                            );
                        }
                    }
                    _ => {}
                }
            }

            min_prec = match next_prec {
                Some(v) => v,
                None => return Ok(next_left),
            };

            left = next_left;
        }
    }

    /// Returns `(left, Some(next_prec))` or `(expr, None)`.
    fn parse_bin_op_recursively_inner(
        &mut self,
        left: MaybeParen,
        min_prec: u8,
    ) -> PResult<(MaybeParen, Option<u8>)> {
        const PREC_OF_IN: u8 = 7;

        if self.input.syntax().typescript()
            && PREC_OF_IN > min_prec
            && !self.input.had_line_break_before_cur()
            && is!(self, "as")
        {
            let expr = left;
            let node = if peeked_is!(self, "const") {
                self.input.bump(); // as
                self.input.bump(); // const
                expr
            } else {
                // Type annotation.
                self.next_then_parse_ts_type()?;
                expr
            };

            return self.parse_bin_op_recursively_inner(node, min_prec);
        }

        let ctx = self.ctx();
        // Return left on eof
        let word = match self.input.cur() {
            Some(cur) => cur,
            _ => return Ok((left, None)),
        };
        let op = match *word {
            Word(Word::Keyword(Keyword::In)) if ctx.include_in_expr() => op!("in"),
            Word(Word::Keyword(Keyword::InstanceOf)) => op!("instanceof"),
            Token::BinOp(op) => op.into(),
            _ => {
                return Ok((left, None));
            }
        };

        if op.precedence() <= min_prec {
            return Ok((left, None));
        }
        self.input.bump();

        if let MaybeParen::Expr(left) = &left {
            if let Expr::Unary(UnaryExpr { node_id, .. }) = left.as_ref() {
                if op == op!("**") {
                    // Correct implementation would be returning Ok(left) and
                    // returning "unexpected token '**'" on next.
                    // But it's not useful error message.

                    syntax_error!(
                        self,
                        SyntaxError::UnaryInExp {
                            // FIXME: Use display
                            left: format!("{:?}", left),
                            left_span: get_span!(self, *node_id),
                        }
                    )
                }
            }
        }

        let right = {
            let left_of_right = self.parse_unary_expr(&mut AssignProps::Emit)?;
            self.parse_bin_op_recursively(
                left_of_right,
                if op == op!("**") {
                    // exponential operator is right associative
                    op.precedence() - 1
                } else {
                    op.precedence()
                },
            )?
        };
        /* this check is for all ?? operators
         * a ?? b && c for this example
         * b && c => This is considered as a logical expression in the ast tree
         * a => Identifier
         * so for ?? operator we need to check in this case the right expression to
         * have parenthesis second case a && b ?? c
         * here a && b => This is considered as a logical expression in the ast tree
         * c => identifier
         * so now here for ?? operator we need to check the left expression to have
         * parenthesis if the parenthesis is missing we raise an error and
         * throw it
         */
        if op == op!("??") {
            if let MaybeParen::Expr(left) = &left {
                match left.as_ref() {
                    Expr::Bin(BinExpr { node_id, op, .. })
                        if *op == op!("&&") || *op == op!("||") =>
                    {
                        self.emit_err(
                            get_span!(self, *node_id),
                            SyntaxError::NullishCoalescingWithLogicalOp,
                        );
                    }
                    _ => {}
                }
            }

            if let MaybeParen::Expr(right) = &right {
                match right.as_ref() {
                    Expr::Bin(BinExpr { node_id, op, .. })
                        if *op == op!("&&") || *op == op!("||") =>
                    {
                        self.emit_err(
                            get_span!(self, *node_id),
                            SyntaxError::NullishCoalescingWithLogicalOp,
                        );
                    }
                    _ => {}
                }
            }
        }

        let lo = get_span!(self, left.node_id()).lo();
        let hi = get_span!(self, right.node_id()).hi();
        let span = Span::new(lo, hi);
        let node = Box::new(Expr::Bin(BinExpr {
            node_id: node_id!(self, span),
            op,
            left: left.unwrap(),
            right: right.unwrap(),
        }));

        Ok((node.into(), Some(min_prec)))
    }

    /// Parse unary expression and update expression.
    ///
    /// spec: 'UnaryExpression'
    pub(in crate::parser) fn parse_unary_expr(
        &mut self,
        assign_props: &mut AssignProps,
    ) -> PResult<MaybeParen> {
        trace_cur!(self, parse_unary_expr);

        let start = self.input.cur_pos();

        if self.input.syntax().typescript() && eat!(self, '<') {
            if eat!(self, "const") {
                expect!(self, '>');
                let expr = self.parse_unary_expr(&mut AssignProps::Emit)?;
                return Ok(expr);
            }

            return self.parse_ts_type_assertion();
        }

        // Parse update expression
        if is!(self, "++") || is!(self, "--") {
            let op = if self.input.bump() == tok!("++") {
                op!("++")
            } else {
                op!("--")
            };

            let arg = self.parse_unary_expr(&mut AssignProps::Emit)?;
            let hi = get_span!(self, arg.node_id()).hi();
            let span = Span::new(start, hi);
            self.check_assign_target(arg.inner(), false);

            return Ok(Box::new(Expr::Update(UpdateExpr {
                node_id: node_id!(self, span),
                prefix: true,
                op,
                arg: arg.unwrap(),
            }))
            .into());
        }

        // Parse unary expression
        if is_one_of!(self, "delete", "void", "typeof", '+', '-', '~', '!') {
            let op = match self.input.bump() {
                tok!("delete") => op!("delete"),
                tok!("void") => op!("void"),
                tok!("typeof") => op!("typeof"),
                tok!('+') => op!(unary, "+"),
                tok!('-') => op!(unary, "-"),
                tok!('~') => op!("~"),
                tok!('!') => op!("!"),
                _ => unreachable!(),
            };
            let arg_start = self.input.cur_pos() - BytePos(1);
            let arg = match self.parse_unary_expr(&mut AssignProps::Emit) {
                Ok(expr) => expr,
                Err(err) => {
                    self.emit_error(err);
                    let span = Span::new(arg_start, arg_start);
                    Box::new(Expr::Invalid(Invalid {
                        node_id: node_id!(self, span),
                    }))
                    .into()
                }
            };

            if let MaybeParen::Expr(arg) = &arg {
                if op == op!("delete") {
                    if let Expr::Ident(i) = arg.as_ref() {
                        self.emit_strict_mode_err(get_span!(self, i.node_id), SyntaxError::TS1102)
                    }
                }

                if self.input.syntax().typescript() && op == op!("delete") {
                    match arg.as_ref() {
                        Expr::Member(..) => {}
                        Expr::OptChain(e) if matches!(&*e.expr, Expr::Member(..)) => {}
                        _ => self.emit_err(get_span!(self, arg.node_id()), SyntaxError::TS2703),
                    }
                }
            }

            let hi = get_span!(self, arg.node_id()).hi();
            let span = Span::new(start, hi);
            return Ok(Box::new(Expr::Unary(UnaryExpr {
                node_id: node_id!(self, span),
                op,
                arg: arg.unwrap(),
            }))
            .into());
        }

        if (self.ctx().in_async() || self.syntax().top_level_await()) && is!(self, "await") {
            return self.parse_await_expr().map(From::from);
        }

        let potential_arrow_start = self.potential_arrow_start;

        // UpdateExpression
        let expr = self.parse_lhs_expr(assign_props)?;
        return_if_arrow!(self, potential_arrow_start, expr);

        // Line terminator isn't allowed here.
        if self.input.had_line_break_before_cur() {
            return Ok(expr);
        }

        if is_one_of!(self, "++", "--") {
            self.check_assign_target(expr.inner(), false);

            let op = if self.input.bump() == tok!("++") {
                op!("++")
            } else {
                op!("--")
            };

            let span = span!(self, get_span!(self, expr.node_id()).lo());
            return Ok(Box::new(Expr::Update(UpdateExpr {
                node_id: node_id!(self, span),
                prefix: false,
                op,
                arg: expr.unwrap(),
            }))
            .into());
        }
        Ok(expr)
    }

    pub(crate) fn parse_await_expr(&mut self) -> PResult<Box<Expr>> {
        let start = self.input.cur_pos();

        self.assert_and_bump(&tok!("await"));

        if is!(self, '*') {
            syntax_error!(self, SyntaxError::AwaitStar);
        }

        if is_one_of!(self, ')', ']') && !self.ctx().in_async() {
            return Ok(Box::new(Expr::Ident(
                self.new_ident(js_word!("await"), span!(self, start)),
            )));
        }

        let arg = self.parse_unary_expr(&mut AssignProps::Emit)?.unwrap();
        Ok(Box::new(Expr::Await(AwaitExpr {
            node_id: node_id!(self, span!(self, start)),
            arg,
        })))
    }
}
