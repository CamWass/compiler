use atoms::JsWord;
use rustc_hash::{FxHashMap, FxHashSet};

// TODO: This can probably be optimised/simplfied:

// It is important that the ordering of FIRST_CHAR is as close to NONFIRST_CHAR
// as possible. Using the ASCII ordering is not a good idea. The reason
// is that we cannot use numbers as FIRST_CHAR yet the ACSII value of numbers
// is very small. If we picked numbers first in NONFIRST_CHAR, we would
// end up balancing the huffman tree and result is bad compression.
/** Generate short name with this first character */
static FIRST_CHAR: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ$";

/** These appear after the first character */
static NONFIRST_CHAR: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789$";

pub struct DefaultNameGenerator {
    priority_lookup_map: FxHashMap<u8, CharPriority>,
    reserved_names: FxHashSet<JsWord>,
    name_count: usize,
    first_chars: Vec<CharPriority>,
    non_first_chars: Vec<CharPriority>,
}

impl std::default::Default for DefaultNameGenerator {
    fn default() -> Self {
        Self::new(Default::default())
    }
}

impl DefaultNameGenerator {
    pub fn new(reserved_names: FxHashSet<JsWord>) -> Self {
        let mut priority_lookup_map =
            FxHashMap::with_capacity_and_hasher(NONFIRST_CHAR.len(), Default::default());
        let mut order = 0;
        for c in NONFIRST_CHAR.bytes() {
            priority_lookup_map.insert(c, CharPriority::new(c, order));
            order += 1;
        }
        let mut gen = Self {
            priority_lookup_map,
            reserved_names,
            name_count: 0,
            first_chars: Vec::new(),
            non_first_chars: Vec::new(),
        };

        // build the character arrays to use
        gen.first_chars = gen.reserve_characters(FIRST_CHAR);
        gen.non_first_chars = gen.reserve_characters(NONFIRST_CHAR);
        gen.first_chars.sort_unstable();
        gen.non_first_chars.sort_unstable();

        gen
    }

    /**
     * Provides the array of available characters based on the specified arrays.
     * @param chars The list of characters that are legal
     * @param reservedCharacters The characters that should not be used
     * @return An array of characters to use. Will return the chars array if
     *    reservedCharacters is null or empty, otherwise creates a new array.
     */
    fn reserve_characters(&self, chars: &str) -> Vec<CharPriority> {
        chars
            .bytes()
            .map(|c| *self.priority_lookup_map.get(&c).unwrap())
            .collect()
    }

    /**
     * Generates the next short name.
     */
    pub fn generate_next_name(&mut self) -> JsWord {
        loop {
            let mut name = String::new();
            let mut i = self.name_count;

            {
                let pos = i % self.first_chars.len();
                name.push(self.first_chars[pos].name as char);
                i /= self.first_chars.len();
            }

            while i > 0 {
                i -= 1;
                let pos = i % self.non_first_chars.len();
                name.push(self.non_first_chars[pos].name as char);
                i /= self.non_first_chars.len();
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

    /**
     * Generates the next short name and reserves it so it can't be used for future names.
     */
    pub fn generate_and_reserve_next_name(&mut self) -> JsWord {
        let new_name = self.generate_next_name();
        self.reserved_names.insert(new_name.clone());
        new_name
    }
}

/**
 * Represents a char that can be used in renaming as well as how often that char appears in the
 * generated code.
 */
#[derive(PartialEq, Eq, Copy, Clone)]
struct CharPriority {
    name: u8,
    occurrence: usize,

    // This is a tie-breaker when two chars occurrence count is the same.
    // When that happens, the 'natural' order prevails.
    order: usize,
}
impl CharPriority {
    pub fn new(name: u8, order: usize) -> Self {
        Self {
            name,
            order,
            occurrence: 0,
        }
    }
}

impl std::cmp::Ord for CharPriority {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // Start out by putting the element with more occurrence first.
        let result = self.occurrence.cmp(&other.occurrence);
        if result.is_eq() {
            // If there is a tie, follow the order of FIRST_CHAR and NONFIRST_CHAR.
            self.order.cmp(&other.order)
        } else {
            result
        }
    }
}

impl std::cmp::PartialOrd for CharPriority {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

// TODO: tests from closure
