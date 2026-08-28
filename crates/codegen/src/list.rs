#![allow(non_upper_case_globals)]
use bitflags::bitflags;

use crate::text_writer::Punct;

bitflags! {
    /// Represents the formatting rule for a list of nodes.
    #[derive(PartialEq, Eq, Copy, Clone)]
    pub struct ListFormat: u16 {
        /// Default value.
        const None = 0;

        // Line separators
        /// Prints the list on a single line (default).
        const SingleLine = 0;
        /// Prints the list on multiple lines.
        const MultiLine = 1 << 0;
        /// Prints the list using line preservation if possible.
        const PreserveLines = 1 << 1;
        const LinesMask = Self::SingleLine.bits() | Self::MultiLine.bits() | Self::PreserveLines.bits();
        // Delimiters
        /// Each list item is comma (",") delimited.
        const CommaDelimited = 1 << 2;
        const DelimitersMask = Self::CommaDelimited.bits();

        /// Write a trailing comma (",") if present.
        const AllowTrailingComma = 1 << 3;

        // Whitespace
        /// The list should be indented.
        const Indented = 1 << 4;
        /// Inserts a space after the opening brace and before the closing
        /// brace.
        const SpaceBetweenBraces = 1 << 5;
        /// Inserts a space between each sibling node.
        const SpaceBetweenSiblings = 1 << 6;

        // Brackets/Braces
        /// The list is surrounded by "{" and "}".
        const Braces = 1 << 7;
        /// The list is surrounded by "(" and ")".
        const Parenthesis = 1 << 8;
        /// The list is surrounded by "[" and "]".
        const SquareBrackets = 1 << 9;
        const BracketsMask = Self::Braces.bits() | Self::Parenthesis.bits() | Self::SquareBrackets.bits();

        /// Do not emit brackets if the list is undefined.
        const OptionalIfUndefined = 1 << 10;
        /// Do not emit brackets if the list is empty.
        const OptionalIfEmpty = 1 << 11;

        // Others
        /// Do not emit a trailing NewLine for a MultiLine list.
        const NoTrailingNewLine = 1 << 12;
        /// If the literal is empty, do not add spaces between braces.
        const NoSpaceIfEmpty = 1 << 13;
        const ForceTrailingComma = 1 << 14;

        // Optimisation.
        const CanSkipTrailingComma = 1 << 15;

        // Precomputed Formats
        const ObjectBindingPatternElements = Self::SingleLine.bits()
            | Self::SpaceBetweenBraces.bits()
            | Self::CommaDelimited.bits()
            | Self::SpaceBetweenSiblings.bits()
            | Self::NoSpaceIfEmpty.bits();
        const ArrayBindingPatternElements = Self::SingleLine.bits()
            | Self::CommaDelimited.bits()
            | Self::SpaceBetweenSiblings.bits()
            | Self::NoSpaceIfEmpty.bits();
        const ObjectLiteralExpressionProperties = Self::MultiLine.bits()
            | Self::CommaDelimited.bits()
            | Self::SpaceBetweenSiblings.bits()
            | Self::SpaceBetweenBraces.bits()
            | Self::Indented.bits()
            | Self::Braces.bits()
            | Self::NoSpaceIfEmpty.bits();
        const ArrayLiteralExpressionElements = Self::PreserveLines.bits()
            | Self::CommaDelimited.bits()
            | Self::SpaceBetweenSiblings.bits()
            | Self::Indented.bits()
            | Self::SquareBrackets.bits();
        const CommaListElements = Self::CommaDelimited.bits() | Self::SpaceBetweenSiblings.bits() | Self::SingleLine.bits();
        const CallExpressionArguments = Self::CommaDelimited.bits() | Self::SpaceBetweenSiblings.bits() | Self::SingleLine.bits() | Self::Parenthesis.bits();
        const NewExpressionArguments = Self::CommaDelimited.bits()
            | Self::SpaceBetweenSiblings.bits()
            | Self::SingleLine.bits()
            | Self::Parenthesis.bits()
            | Self::OptionalIfUndefined.bits();
        const MultiLineBlockStatements = Self::Indented.bits() | Self::MultiLine.bits();
        const VariableDeclarationList = Self::CommaDelimited.bits() | Self::SpaceBetweenSiblings.bits() | Self::SingleLine.bits();
        const ClassMembers = Self::Indented.bits() | Self::MultiLine.bits();
        const CaseBlockClauses = Self::Indented.bits() | Self::MultiLine.bits();
        const NamedImportsOrExportsElements = Self::CommaDelimited.bits()
            | Self::SpaceBetweenSiblings.bits()
            | Self::AllowTrailingComma.bits()
            | Self::SingleLine.bits()
            | Self::SpaceBetweenBraces.bits();
        const CaseOrDefaultClauseStatements = Self::Indented.bits() | Self::MultiLine.bits() | Self::NoTrailingNewLine.bits() | Self::OptionalIfEmpty.bits();
        const Parameters = Self::CommaDelimited.bits() | Self::SpaceBetweenSiblings.bits() | Self::SingleLine.bits() | Self::Parenthesis.bits();
    }
}

impl ListFormat {
    pub fn opening_bracket(self) -> Punct {
        match self & ListFormat::BracketsMask {
            ListFormat::Braces => Punct::LBrace,
            ListFormat::Parenthesis => Punct::LParen,
            ListFormat::SquareBrackets => Punct::LBracket,
            _ => unreachable!(),
        }
    }
    pub fn closing_bracket(self) -> Punct {
        match self & ListFormat::BracketsMask {
            ListFormat::Braces => Punct::RBrace,
            ListFormat::Parenthesis => Punct::RParen,
            ListFormat::SquareBrackets => Punct::RBracket,
            _ => unreachable!(),
        }
    }
}
