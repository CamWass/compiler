use super::sip128::SipHasher128;
use std::{hash::Hasher, marker::PhantomData};

/// When hashing something that ends up affecting properties like symbol names,
/// we want these symbol names to be calculated independently of other factors
/// like what architecture you're compiling *from*.
///
/// To that end we always convert integers to little-endian format before
/// hashing and the architecture dependent `isize` and `usize` types are
/// extended to 64 bits if needed.
pub struct StableHasher<W> {
    state: SipHasher128,
    bytes_hashed: u64,
    width: PhantomData<W>,
}

impl<W: StableHasherResult> ::std::fmt::Debug for StableHasher<W> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
        write!(f, "{:?}", self.state)
    }
}

pub trait StableHasherResult: Sized {
    fn finish(hasher: StableHasher<Self>) -> Self;
}

impl<W: StableHasherResult> StableHasher<W> {
    pub fn new() -> Self {
        StableHasher {
            state: SipHasher128::new_with_keys(0, 0),
            bytes_hashed: 0,
            width: PhantomData,
        }
    }

    pub fn finish(self) -> W {
        W::finish(self)
    }
}

impl StableHasherResult for u128 {
    fn finish(hasher: StableHasher<Self>) -> Self {
        let (_0, _1) = hasher.finalize();
        (_0 as u128) | ((_1 as u128) << 64)
    }
}

impl StableHasherResult for u64 {
    fn finish(hasher: StableHasher<Self>) -> Self {
        hasher.finalize().0
    }
}

impl<W> StableHasher<W> {
    #[inline]
    pub fn finalize(self) -> (u64, u64) {
        self.state.finish128()
    }
}

impl<W> Hasher for StableHasher<W> {
    fn finish(&self) -> u64 {
        panic!("use StableHasher::finalize instead");
    }

    #[inline]
    fn write(&mut self, bytes: &[u8]) {
        self.state.write(bytes);
        self.bytes_hashed += bytes.len() as u64;
    }

    #[inline]
    fn write_u8(&mut self, i: u8) {
        self.state.write_u8(i);
        self.bytes_hashed += 1;
    }

    #[inline]
    fn write_u16(&mut self, i: u16) {
        self.state.write_u16(i.to_le());
        self.bytes_hashed += 2;
    }

    #[inline]
    fn write_u32(&mut self, i: u32) {
        self.state.write_u32(i.to_le());
        self.bytes_hashed += 4;
    }

    #[inline]
    fn write_u64(&mut self, i: u64) {
        self.state.write_u64(i.to_le());
        self.bytes_hashed += 8;
    }

    #[inline]
    fn write_u128(&mut self, i: u128) {
        self.state.write_u128(i.to_le());
        self.bytes_hashed += 16;
    }

    #[inline]
    fn write_usize(&mut self, i: usize) {
        // Always treat usize as u64 so we get the same results on 32 and 64 bit
        // platforms. This is important for symbol hashes when cross compiling,
        // for example.
        self.state.write_u64((i as u64).to_le());
        self.bytes_hashed += 8;
    }

    #[inline]
    fn write_i8(&mut self, i: i8) {
        self.state.write_i8(i);
        self.bytes_hashed += 1;
    }

    #[inline]
    fn write_i16(&mut self, i: i16) {
        self.state.write_i16(i.to_le());
        self.bytes_hashed += 2;
    }

    #[inline]
    fn write_i32(&mut self, i: i32) {
        self.state.write_i32(i.to_le());
        self.bytes_hashed += 4;
    }

    #[inline]
    fn write_i64(&mut self, i: i64) {
        self.state.write_i64(i.to_le());
        self.bytes_hashed += 8;
    }

    #[inline]
    fn write_i128(&mut self, i: i128) {
        self.state.write_i128(i.to_le());
        self.bytes_hashed += 16;
    }

    #[inline]
    fn write_isize(&mut self, i: isize) {
        // Always treat isize as i64 so we get the same results on 32 and 64 bit
        // platforms. This is important for symbol hashes when cross compiling,
        // for example.
        self.state.write_i64((i as i64).to_le());
        self.bytes_hashed += 8;
    }
}
