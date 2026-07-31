use atoms::JsWord;
use rustc_hash::FxHashSet;

// TODO: each generator usually generates a small set of names - we could
// pregenerate some reasonable number and just maintain an index into that array.
// TODO: why can't we use '_' as a first char?
// It is important that the ordering of FIRST_CHAR is as close to NON_FIRST_CHAR
// as possible. Using the ASCII ordering is not a good idea. The reason
// is that we cannot use numbers as FIRST_CHAR yet the ASCII value of numbers
// is very small. If we picked numbers first in NON_FIRST_CHAR, we would
// end up balancing the huffman tree and result is bad compression.
/// Generate short name with this first character.
static FIRST_CHAR: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ$";

/// These appear after the first character.
static NON_FIRST_CHAR: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789$";

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
                let pos = i % NON_FIRST_CHAR.len();
                name.push(NON_FIRST_CHAR.as_bytes()[pos] as char);
                i /= NON_FIRST_CHAR.len();
            }

            self.name_count += 1;

            if is_reserved(&name) {
                continue;
            }

            let name = JsWord::from(name);

            if !self.reserved_names.contains(&name) {
                return name;
            }
        }
    }
}

const RESERVED_STRINGS: [&str; 48] = [
    "break",
    "case",
    "catch",
    "class",
    "const",
    "continue",
    "debugger",
    "default",
    "delete",
    "do",
    "else",
    "enum",
    "export",
    "extends",
    "false",
    "finally",
    "for",
    "function",
    "if",
    "import",
    "in",
    "instanceof",
    "new",
    "null",
    "package",
    "return",
    "super",
    "switch",
    "this",
    "throw",
    "true",
    "try",
    "typeof",
    "var",
    "void",
    "while",
    "with",
    "implements",
    "interface",
    "let",
    "package",
    "private",
    "protected",
    "public",
    "static",
    "yield",
    "eval",
    "arguments",
];

const fn min_len(arr: &[&str; 48]) -> usize {
    let mut min = usize::MAX;
    let mut i = 0;
    while i < arr.len() {
        let len = arr[i].len();
        if len < min {
            min = len;
        }
        i += 1;
    }
    min
}

const MIN_RESERVED_LEN: usize = min_len(&RESERVED_STRINGS);

fn is_reserved(name: &str) -> bool {
    name.len() >= MIN_RESERVED_LEN && RESERVED_STRINGS.contains(&name)
}

// TODO: tests from closure

#[test]
fn test_collision_with_past_names() {
    let mut generator = DefaultNameGenerator::default();
    let names = (0..1_000_000)
        .map(|_| generator.generate_next_name())
        .collect::<FxHashSet<_>>();
    assert_eq!(names.len(), 1_000_000);
}

#[test]
fn test_no_reserved_keywords() {
    let mut generator = DefaultNameGenerator::default();
    let names = (0..1_000_000)
        .map(|_| generator.generate_next_name())
        .collect::<FxHashSet<_>>();

    for keyword in RESERVED_STRINGS {
        assert!(
            !names.contains(&JsWord::from(keyword)),
            "Generated reserved word: '{keyword}'"
        );
    }
}
