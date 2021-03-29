use std::{
    hash::Hash,
    ops::{Add, Sub},
};

pub trait Pos {
    fn from_usize(n: usize) -> Self;
    fn to_usize(&self) -> usize;
    fn from_u32(n: u32) -> Self;
    fn to_u32(&self) -> u32;
}

/// A byte offset. Keep this small (currently 32-bits), as AST contains
/// a lot of them.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct BytePos(pub u32);

/// A character offset. Because of multibyte utf8 characters, a byte offset
/// is not equivalent to a character offset. The SourceMap will convert BytePos
/// values to CharPos values as necessary.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct CharPos(pub usize);

// FIXME: Lots of boilerplate in these impls, but so far my attempts to fix
// have been unsuccessful

impl Pos for BytePos {
    #[inline(always)]
    fn from_usize(n: usize) -> BytePos {
        BytePos(n as u32)
    }

    #[inline(always)]
    fn to_usize(&self) -> usize {
        self.0 as usize
    }

    #[inline(always)]
    fn from_u32(n: u32) -> BytePos {
        BytePos(n)
    }

    #[inline(always)]
    fn to_u32(&self) -> u32 {
        self.0
    }
}

impl Add for BytePos {
    type Output = BytePos;

    #[inline(always)]
    fn add(self, rhs: BytePos) -> BytePos {
        BytePos((self.to_usize() + rhs.to_usize()) as u32)
    }
}

impl Sub for BytePos {
    type Output = BytePos;

    #[inline(always)]
    fn sub(self, rhs: BytePos) -> BytePos {
        BytePos((self.to_usize() - rhs.to_usize()) as u32)
    }
}

impl Pos for CharPos {
    #[inline(always)]
    fn from_usize(n: usize) -> CharPos {
        CharPos(n)
    }

    #[inline(always)]
    fn to_usize(&self) -> usize {
        self.0
    }

    #[inline(always)]
    fn from_u32(n: u32) -> CharPos {
        CharPos(n as usize)
    }

    #[inline(always)]
    fn to_u32(&self) -> u32 {
        self.0 as u32
    }
}

impl Add for CharPos {
    type Output = CharPos;

    #[inline(always)]
    fn add(self, rhs: CharPos) -> CharPos {
        CharPos(self.to_usize() + rhs.to_usize())
    }
}

impl Sub for CharPos {
    type Output = CharPos;

    #[inline(always)]
    fn sub(self, rhs: CharPos) -> CharPos {
        CharPos(self.to_usize() - rhs.to_usize())
    }
}

/// Implementation of [Input].
#[derive(Clone)]
pub struct StringInput<'a> {
    start_pos: BytePos,
    last_pos: BytePos,
    /// Current cursor
    iter: std::str::CharIndices<'a>,
    orig: &'a str,
    /// Original start position.
    orig_start: BytePos,
}

impl<'a> StringInput<'a> {
    /// `start` and `end` can be arbitrary value, but start should be less than
    /// or equal to end.
    ///
    ///
    /// `swc` get this value from [SourceMap] because code generator depends on
    /// some methods of [SourceMap].
    /// If you are not going to use methods from
    /// [SourceMap], you may use any value.
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

    fn cur_pos(&mut self) -> BytePos {
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
    fn slice(&mut self, start: BytePos, end: BytePos) -> &str {
        assert!(start <= end, "Cannot slice {:?}..{:?}", start, end);
        let s = self.orig;

        let start_idx = (start - self.orig_start).0 as usize;
        let end_idx = (end - self.orig_start).0 as usize;

        let ret = &s[start_idx..end_idx];

        self.iter = s[end_idx..].char_indices();
        self.last_pos = end;
        self.start_pos = end;

        ret
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

    fn cur_pos(&mut self) -> BytePos;

    fn last_pos(&self) -> BytePos;

    fn slice(&mut self, start: BytePos, end: BytePos) -> &str;

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
