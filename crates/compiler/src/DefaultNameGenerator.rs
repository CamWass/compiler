use atoms::JsWord;
use rustc_hash::FxHashSet;

// It is important that the ordering of FIRST_CHAR is as close to NONFIRST_CHAR
// as possible. Using the ASCII ordering is not a good idea. The reason
// is that we cannot use numbers as FIRST_CHAR yet the ACSII value of numbers
// is very small. If we picked numbers first in NONFIRST_CHAR, we would
// end up balancing the huffman tree and result is bad compression.
/// Generate short name with this first character.
static FIRST_CHAR: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ$";

/// These appear after the first character.
static NONFIRST_CHAR: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789$";

#[derive(Default)]
pub struct DefaultNameGenerator {
    reserved_names: FxHashSet<JsWord>,
    name_count: usize,
}

impl DefaultNameGenerator {
    pub fn new(reserved_names: FxHashSet<JsWord>) -> Self {
        Self {
            reserved_names,
            name_count: 0,
        }
    }

    /// Generates the next short name.
    pub fn generate_next_name(&mut self) -> JsWord {
        loop {
            let mut name = String::new();
            let mut i = self.name_count;

            {
                let pos = i % FIRST_CHAR.len();
                name.push(FIRST_CHAR.as_bytes()[pos] as char);
                i /= FIRST_CHAR.len();
            }

            while i > 0 {
                i -= 1;
                let pos = i % NONFIRST_CHAR.len();
                name.push(NONFIRST_CHAR.as_bytes()[pos] as char);
                i /= NONFIRST_CHAR.len();
            }

            self.name_count += 1;

            let name = JsWord::from(name);

            // Make sure it's not a JS keyword or reserved name.
            // if !(TokenStream.isKeyword(name) || self.reservedNames.contains(name) || isBadName(name)) {
            if !self.reserved_names.contains(&name) {
                return name;
            }
        }
    }
}

// TODO: tests from closure

#[test]
fn test_collision_with_past_names() {
    let mut gen = DefaultNameGenerator::default();
    let names = (0..1_000_000)
        .map(|_| gen.generate_next_name())
        .collect::<FxHashSet<_>>();
    assert_eq!(names.len(), 1_000_000);
}
