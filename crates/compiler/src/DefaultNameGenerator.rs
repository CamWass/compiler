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
    priorityLookupMap: FxHashMap<u8, CharPriority>,
    reservedNames: FxHashSet<JsWord>,
    nameCount: usize,
    firstChars: Vec<CharPriority>,
    nonFirstChars: Vec<CharPriority>,
}

impl std::default::Default for DefaultNameGenerator {
    fn default() -> Self {
        Self::new(Default::default())
    }
}

impl DefaultNameGenerator {
    pub fn new(reservedNames: FxHashSet<JsWord>) -> Self {
        let mut priorityLookupMap =
            FxHashMap::with_capacity_and_hasher(NONFIRST_CHAR.len(), Default::default());
        let mut order = 0;
        for c in NONFIRST_CHAR.bytes() {
            priorityLookupMap.insert(c, CharPriority::new(c, order));
            order += 1;
        }
        let mut gen = Self {
            priorityLookupMap,
            reservedNames,
            nameCount: 0,
            firstChars: Vec::new(),
            nonFirstChars: Vec::new(),
        };

        // build the character arrays to use
        gen.firstChars = gen.reserveCharacters(FIRST_CHAR);
        gen.nonFirstChars = gen.reserveCharacters(NONFIRST_CHAR);
        gen.firstChars.sort_unstable();
        gen.nonFirstChars.sort_unstable();

        gen
    }

    /**
     * Provides the array of available characters based on the specified arrays.
     * @param chars The list of characters that are legal
     * @param reservedCharacters The characters that should not be used
     * @return An array of characters to use. Will return the chars array if
     *    reservedCharacters is null or empty, otherwise creates a new array.
     */
    fn reserveCharacters(&self, chars: &str) -> Vec<CharPriority> {
        chars
            .bytes()
            .map(|c| *self.priorityLookupMap.get(&c).unwrap())
            .collect()
    }

    /**
     * Generates the next short name.
     */
    pub fn generateNextName(&mut self) -> JsWord {
        loop {
            let mut name = String::new();
            let mut i = self.nameCount;

            {
                let pos = i % self.firstChars.len();
                name.push(self.firstChars[pos].name as char);
                i /= self.firstChars.len();
            }

            while i > 0 {
                i -= 1;
                let pos = i % self.nonFirstChars.len();
                name.push(self.nonFirstChars[pos].name as char);
                i /= self.nonFirstChars.len();
            }

            self.nameCount += 1;

            let name = JsWord::from(name);

            // Make sure it's not a JS keyword or reserved name.
            // if !(TokenStream.isKeyword(name) || self.reservedNames.contains(name) || isBadName(name)) {
            if !self.reservedNames.contains(&name) {
                return name;
            }
        }
    }

    /**
     * Generates the next short name and reserves it so it can't be used for future names.
     */
    pub fn generateAndReserveNextName(&mut self) -> JsWord {
        let new_name = self.generateNextName();
        self.reservedNames.insert(new_name.clone());
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
