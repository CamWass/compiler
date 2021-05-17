use crate::{pos::BytePos, Pos};
use std::str;

/// Implementation of [Input].
#[derive(Clone)]
pub struct StringInput<'a> {
    start_pos: BytePos,
    last_pos: BytePos,
    /// Current cursor
    iter: str::CharIndices<'a>,
    orig: &'a str,
    /// Original start position.
    orig_start: BytePos,
}

impl<'a> StringInput<'a> {
    pub fn new(src: &'a str, start: BytePos, end: BytePos) -> Self {
        assert!(start <= end);

        StringInput {
            start_pos: start,
            last_pos: start,
            orig: src,
            iter: src.char_indices(),
            orig_start: start,
        }
    }
}

impl<'a> Input for StringInput<'a> {
    #[inline]
    fn cur(&mut self) -> Option<char> {
        self.iter.clone().nth(0).map(|i| i.1)
    }

    #[inline]
    fn peek(&mut self) -> Option<char> {
        self.iter.clone().nth(1).map(|i| i.1)
    }

    #[inline]
    fn peek_ahead(&mut self) -> Option<char> {
        self.iter.clone().nth(2).map(|i| i.1)
    }

    #[inline]
    fn bump(&mut self) {
        if let Some((i, c)) = self.iter.next() {
            self.last_pos = self.start_pos + BytePos((i + c.len_utf8()) as u32);
        } else {
            unreachable!("bump should not be called when cur() == None");
        }
    }

    #[inline]
    fn is_at_start(&self) -> bool {
        self.orig_start == self.last_pos
    }

    fn cur_pos(&self) -> BytePos {
        self.iter
            .clone()
            .next()
            .map(|(p, _)| self.start_pos + BytePos(p as u32))
            .unwrap_or(self.last_pos)
    }

    #[inline]
    fn last_pos(&self) -> BytePos {
        self.last_pos
    }

    #[inline]
    fn slice_to_cur(&mut self, start: usize) -> &str {
        &self.orig[start..self.cur_pos().to_usize()]
    }

    fn uncons_while<F>(&mut self, mut pred: F) -> &str
    where
        F: FnMut(char) -> bool,
    {
        let s = self.iter.as_str();
        let mut last = 0;

        for (i, c) in s.char_indices() {
            if pred(c) {
                last = i + c.len_utf8();
            } else {
                break;
            }
        }
        let ret = &s[..last];

        self.last_pos = self.last_pos + BytePos(last as _);
        self.start_pos = self.last_pos;
        self.iter = s[last..].char_indices();

        ret
    }

    fn find<F>(&mut self, mut pred: F) -> Option<BytePos>
    where
        F: FnMut(char) -> bool,
    {
        let s = self.iter.as_str();
        let mut last = 0;

        for (i, c) in s.char_indices() {
            if pred(c) {
                last = i + c.len_utf8();
                break;
            }
        }
        if last == 0 {
            return None;
        }

        self.last_pos = self.last_pos + BytePos(last as _);
        self.start_pos = self.last_pos;
        self.iter = s[last..].char_indices();

        Some(self.last_pos)
    }

    #[inline]
    fn reset_to(&mut self, to: BytePos) {
        let orig = self.orig;
        let idx = (to - self.orig_start).0 as usize;

        let s = &orig[idx..];
        self.iter = s.char_indices();
        self.start_pos = to;
        self.last_pos = to;
    }

    #[inline]
    fn is_byte(&mut self, c: u8) -> bool {
        if self.iter.as_str().len() == 0 {
            false
        } else {
            self.iter.as_str().as_bytes()[0] == c
        }
    }
}

pub trait Input: Clone {
    fn cur(&mut self) -> Option<char>;
    fn peek(&mut self) -> Option<char>;
    fn peek_ahead(&mut self) -> Option<char>;
    fn bump(&mut self);

    fn is_at_start(&self) -> bool;

    fn cur_pos(&self) -> BytePos;

    fn last_pos(&self) -> BytePos;

    fn slice_to_cur(&mut self, start: usize) -> &str;

    /// Takes items from stream, testing each one with predicate. returns the
    /// range of items which passed predicate.
    fn uncons_while<F>(&mut self, f: F) -> &str
    where
        F: FnMut(char) -> bool;

    /// This method modifies [last_pos()] and [cur_pos()].
    fn find<F>(&mut self, f: F) -> Option<BytePos>
    where
        F: FnMut(char) -> bool;

    fn reset_to(&mut self, to: BytePos);

    /// Implementors can override the method to make it faster.
    #[inline]
    fn is_byte(&mut self, c: u8) -> bool {
        match self.cur() {
            Some(ch) => ch == c as char,
            _ => false,
        }
    }

    /// Implementors can override the method to make it faster.
    #[inline]
    fn eat_byte(&mut self, c: u8) -> bool {
        if self.is_byte(c) {
            self.bump();
            true
        } else {
            false
        }
    }
}
