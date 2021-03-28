use std::{
    hash::Hash,
    ops::{Add, Sub},
};

// A byte offset.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct BytePos(pub u32);

impl BytePos {
    fn from_usize(n: usize) -> BytePos {
        BytePos(n as u32)
    }

    fn to_usize(&self) -> usize {
        self.0 as usize
    }

    fn from_u32(n: u32) -> BytePos {
        BytePos(n)
    }

    fn to_u32(&self) -> u32 {
        self.0
    }
}

impl Add for BytePos {
    type Output = BytePos;

    fn add(self, rhs: BytePos) -> BytePos {
        BytePos((self.to_usize() + rhs.to_usize()) as u32)
    }
}

impl Sub for BytePos {
    type Output = BytePos;

    fn sub(self, rhs: BytePos) -> BytePos {
        BytePos((self.to_usize() - rhs.to_usize()) as u32)
    }
}

// #[derive(Clone)]
pub struct Input<'a> {
    start_pos: BytePos,
    last_pos: BytePos,
    /// Current cursor
    // TODO: Why not std::str::Chars ?
    iter: std::str::CharIndices<'a>,
    orig: &'a str,
    /// Original start position.
    orig_start: BytePos,
}

impl<'a> Input<'a> {
    pub fn new(src: &'a str, start: BytePos, end: BytePos) -> Self {
        assert!(start <= end);

        Input {
            start_pos: start,
            last_pos: start,
            orig: src,
            iter: src.char_indices(),
            orig_start: start,
        }
    }
}

impl<'a> Input<'a> {
    // #[inline]
    pub fn cur(&mut self) -> Option<char> {
        self.iter.clone().nth(0).map(|i| i.1)
    }

    // #[inline]
    pub fn peek(&mut self) -> Option<char> {
        self.iter.clone().nth(1).map(|i| i.1)
    }

    // #[inline]
    pub fn peek_ahead(&mut self) -> Option<char> {
        self.iter.clone().nth(2).map(|i| i.1)
    }

    // #[inline]
    pub fn bump(&mut self) {
        if let Some((i, c)) = self.iter.next() {
            self.last_pos = self.start_pos + BytePos((i + c.len_utf8()) as u32);
        } else {
            unreachable!("bump should not be called when cur() == None");
        }
    }

    // #[inline]
    pub fn is_at_start(&self) -> bool {
        self.orig_start == self.last_pos
    }

    pub fn cur_pos(&mut self) -> BytePos {
        self.iter
            .clone()
            .next()
            .map(|(p, _)| self.start_pos + BytePos(p as u32))
            .unwrap_or(self.last_pos)
    }

    // #[inline]
    fn last_pos(&self) -> BytePos {
        self.last_pos
    }

    // #[inline]
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

    // #[inline]
    fn reset_to(&mut self, to: BytePos) {
        let orig = self.orig;
        let idx = (to - self.orig_start).0 as usize;

        let s = &orig[idx..];
        self.iter = s.char_indices();
        self.start_pos = to;
        self.last_pos = to;
    }

    // #[inline]
    fn is_byte(&mut self, c: u8) -> bool {
        if self.iter.as_str().len() == 0 {
            false
        } else {
            self.iter.as_str().as_bytes()[0] == c
        }
    }
}
