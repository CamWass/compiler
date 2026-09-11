use std::{
    borrow::Cow,
    hash::{BuildHasher, BuildHasherDefault},
};

use common::Span;
use hashbrown::HashTable;
use index::vec::IndexVec;
use rustc_hash::FxHasher;
use serde::Serialize;

index::newtype_index!(pub NodeId, Serialize);

impl NodeId {
    pub const DUMMY: NodeId = NodeId::MAX;
}

index::newtype_index!(pub NameId, Serialize);

impl NameId {
    pub const DUMMY: NameId = NameId::MAX;

    pub fn is_unresolved(self) -> bool {
        (self.0 & (1 << (u32::BITS - 1))) == 0
    }
}

// A delightful consequence of PartialEq not being const at the time of
// writing...
const fn str_eq(a: &str, b: &str) -> bool {
    let a = a.as_bytes();
    let b = b.as_bytes();

    if a.len() != b.len() {
        return false;
    }

    let mut i = 0;
    while i < a.len() {
        if a[i] != b[i] {
            return false;
        }
        i += 1;
    }

    true
}

macro_rules! make_built_ins {
    ($($name:literal),* $(,)?) => {
        // Use $crate::paste so consuming crates don't need `paste` in their Cargo.toml:
        $crate::paste::paste! {
            const BUILT_IN_NAMES: &[&str] = &[$($name),*];

            const fn built_in_index(name: &str) -> u32 {
                let mut i = 0;
                while i < BUILT_IN_NAMES.len() {
                    if str_eq(BUILT_IN_NAMES[i], name) {
                        return i as u32;
                    }
                    i += 1;
                }
                panic!("built_in_index called with unknown built-in name");
            }

            $(
                #[allow(non_upper_case_globals)]
                pub const [< $name _ID >]: NameId =
                    NameId::from_u32(built_in_index($name));
            )*

            #[macro_export]
            macro_rules! id_for_built_in {
                $(
                    ($name) => {
                        $crate::[< $name _ID >]
                    };
                )*
            }
        }
    };
}

// TODO: maybe re-order this so e.g. reserved names are in a dense range, so we
// use simplified condition checks instead of matching.
// Also consider lookup tables, since the built-ins fit within a u8.
make_built_ins!(
    // The lexer relies on this section having the lowest values (i.e. starting
    // at zero) and having the same ordering as the corresponding Tokens, so
    // converting between them is a no-op.
    // --Start--
    "await",
    "break",
    "case",
    "catch",
    "continue",
    "debugger",
    "default",
    "do",
    "else",
    "finally",
    "for",
    "function",
    "if",
    "return",
    "switch",
    "throw",
    "try",
    "var",
    "let",
    "const",
    "while",
    "with",
    "new",
    "this",
    "super",
    "class",
    "extends",
    "export",
    "import",
    "yield",
    "in",
    "instanceof",
    "typeof",
    "void",
    "delete",
    "null",
    "true",
    "false",
    "async",
    //
    "as",
    "from",
    "of",
    "static",
    "target",
    "asserts",
    "implements",
    "is",
    "keyof",
    "unique",
    "object",
    "global",
    "enum",
    "readonly",
    "abstract",
    "infer",
    "any",
    "boolean",
    "bigint",
    "intrinsic",
    "never",
    "number",
    "string",
    "symbol",
    "unknown",
    "interface",
    "declare",
    "undefined",
    "meta",
    "type",
    "assert",
    "get",
    "set",
    "public",
    "protected",
    "private",
    "package",
    "override",
    // --End--
    "__defineGetter__",
    "__defineSetter__",
    "__lookupGetter__",
    "__lookupSetter__",
    "__proto__",
    "abs",
    "acos",
    "acosh",
    "arguments",
    "Array",
    "asin",
    "asinh",
    "at",
    "atan",
    "atan2",
    "atanh",
    "BigInt",
    "Boolean",
    "cbrt",
    "ceil",
    "charAt",
    "charCodeAt",
    "codePointAt",
    "concat",
    "constructor",
    "cos",
    "cosh",
    "Date",
    "dotAll",
    "endsWith",
    "Error",
    "eval",
    "exec",
    "exp",
    "expm1",
    "flags",
    "floor",
    "hasIndices",
    "hasOwnProperty",
    "hypot",
    "ignoreCase",
    "includes",
    "indexOf",
    "Infinity",
    "isPrototypeOf",
    "isWellFormed",
    "lastIndex",
    "lastIndexOf",
    "length",
    "localeCompare",
    "log",
    "log10",
    "log1p",
    "log2",
    "match",
    "matchAll",
    "Math",
    "max",
    "min",
    "module",
    "multiline",
    "namespace",
    "NaN",
    "normalize",
    "Number",
    "Object",
    "padEnd",
    "padStart",
    "pow",
    "propertyIsEnumerable",
    "prototype",
    "random",
    "RegExp",
    "repeat",
    "replace",
    "replaceAll",
    "round",
    "search",
    "sign",
    "sin",
    "sinh",
    "slice",
    "source",
    "split",
    "sqrt",
    "startsWith",
    "sticky",
    "String",
    "substr",
    "substring",
    "tan",
    "tanh",
    "test",
    "toExponential",
    "toFixed",
    "toLocaleLowerCase",
    "toLocaleString",
    "toLocaleUpperCase",
    "toLowerCase",
    "toPrecision",
    "toString",
    "toUpperCase",
    "toWellFormed",
    "trim",
    "trimEnd",
    "trimStart",
    "trunc",
    "unicode",
    "unicodeSets",
    "valueOf",
    "XMLHttpRequest",
    "_I_N_V_A_L_I_D_",
);

/// Used for dummy identifiers created by the parser during error recovery.
/// Rather than halt parsing on any error, we try to continue parsing, but we
/// have to insert _something_ into the AST, so we create dummy nodes - mainly
/// identifiers with this name.
///
/// Since we don't continue compilation when the parser errors, this name should
/// never escape the parser.
///
/// Having this sentinel 'invalid' name prevents us from having dedicated
/// 'invalid' AST node types that we'd have to explicitly ignore throughout the
/// compiler.
pub const INVALID_IDENT_NAME: NameId = id_for_built_in!("_I_N_V_A_L_I_D_");

#[derive(Debug)]
pub struct ProgramData {
    spans: IndexVec<NodeId, Span>,
    names: IndexVec<NameId, (String, u64)>,
    // Maps NameId -> () (We store IDs as keys, look up strings from `names`)
    name_to_id: HashTable<NameId>,
    hasher: BuildHasherDefault<FxHasher>,
}

impl ProgramData {
    fn new() -> Self {
        let mut name_to_id = HashTable::with_capacity(BUILT_IN_NAMES.len());
        let mut names = IndexVec::with_capacity(BUILT_IN_NAMES.len());

        let hasher = BuildHasherDefault::default();

        // Initialise names and name map with built-in names.
        for &name in BUILT_IN_NAMES {
            let name_hash = hasher.hash_one(name);
            let id = names.push((name.to_string(), name_hash));
            name_to_id.insert_unique(name_hash, id, |&stored_id| {
                if cfg!(debug_assertions) {
                    // We created the HashTable with enough capacity.
                    unreachable!(
                        "Built in name map should not re-hash/re-alloc during initialisation"
                    );
                }

                names[stored_id].1
            });
        }

        Self {
            spans: IndexVec::default(),
            names,
            name_to_id,
            hasher,
        }
    }

    fn new_id(&mut self, span: Span) -> NodeId {
        self.spans.push(span)
    }

    pub fn new_id_from(&mut self, other: NodeId) -> NodeId {
        let other = self.spans[other];
        self.spans.push(other)
    }

    fn get_span(&self, node: NodeId) -> Span {
        self.spans[node]
    }

    fn set_span(&mut self, node: NodeId, span: Span) {
        self.spans[node] = span;
    }

    fn get_id_for_name(&mut self, name: Cow<str>) -> NameId {
        let name_hash = self.hasher.hash_one(&name);

        let entry = self
            .name_to_id
            .find_entry(name_hash, |&id| self.names[id].0 == name);

        match entry {
            Ok(occ) => *occ.get(),
            Err(absent) => {
                let id = self.names.push((name.to_string(), name_hash));

                absent
                    .into_table()
                    .insert_unique(name_hash, id, |&stored_id| self.names[stored_id].1);

                id
            }
        }
    }

    fn get_name_and_hash_for_id(&self, id: NameId) -> (&str, u64) {
        let id = NameId::from_u32(id.0 & !(1 << (u32::BITS - 1)));
        (&self.names[id].0, self.names[id].1)
    }

    fn get_name_for_id(&self, id: NameId) -> &str {
        self.get_name_and_hash_for_id(id).0
    }

    // TODO: ideally this isn't pub
    pub fn mark_resolved(id: NameId) -> NameId {
        NameId::from_u32(id.0 | 1 << (u32::BITS - 1))
    }
}

#[derive(Debug)]
pub struct ParserProgramData(ProgramData);

impl Default for ParserProgramData {
    fn default() -> Self {
        Self(ProgramData::new())
    }
}

impl ParserProgramData {
    pub fn data(&mut self) -> &mut ProgramData {
        &mut self.0
    }

    pub fn into_transformer_program_data(self) -> TransformerProgramData {
        TransformerProgramData(self.0)
    }

    pub fn into_codegen_program_data(self) -> CodegenProgramData {
        CodegenProgramData(self.0)
    }

    pub fn new_id(&mut self, span: Span) -> NodeId {
        self.0.new_id(span)
    }

    pub fn new_id_from(&mut self, other: NodeId) -> NodeId {
        self.0.new_id_from(other)
    }

    pub fn get_span(&self, node: NodeId) -> Span {
        self.0.get_span(node)
    }

    pub fn set_span(&mut self, node: NodeId, span: Span) {
        self.0.set_span(node, span);
    }

    pub fn intern_name(&mut self, name: Cow<str>) -> NameId {
        self.0.get_id_for_name(name)
    }

    pub fn get_name_text(&self, name: NameId) -> &str {
        self.0.get_name_for_id(name)
    }
}

#[derive(Debug)]
pub struct TransformerProgramData(ProgramData);

impl TransformerProgramData {
    pub fn data(&mut self) -> &mut ProgramData {
        &mut self.0
    }

    pub fn into_codegen_program_data(self) -> CodegenProgramData {
        CodegenProgramData(self.0)
    }

    pub fn into_testing_program_data(self) -> TestingProgramData {
        TestingProgramData(self.0)
    }

    pub fn new_id(&mut self, span: Span) -> NodeId {
        self.0.new_id(span)
    }

    pub fn new_id_from(&mut self, other: NodeId) -> NodeId {
        self.0.new_id_from(other)
    }

    pub fn get_span(&self, node: NodeId) -> Span {
        self.0.get_span(node)
    }

    pub fn intern_name(&mut self, name: Cow<str>) -> NameId {
        self.0.get_id_for_name(name)
    }

    pub fn get_name_text(&self, name: NameId) -> &str {
        self.0.get_name_for_id(name)
    }

    pub fn new_resolved_name(&mut self, name: Cow<str>) -> NameId {
        let name_hash = self.0.hasher.hash_one(&name);
        ProgramData::mark_resolved(self.0.names.push((name.to_string(), name_hash)))
    }

    pub fn new_resolved_name_from(&mut self, id: NameId) -> NameId {
        let (name, hash) = self.0.get_name_and_hash_for_id(id);
        ProgramData::mark_resolved(self.0.names.push((String::from(name), hash)))
    }
}

#[derive(Debug)]
pub struct CodegenProgramData(ProgramData);

impl CodegenProgramData {
    pub fn get_span(&self, node: NodeId) -> Span {
        self.0.get_span(node)
    }

    pub fn get_name_text(&self, name: NameId) -> &str {
        self.0.get_name_for_id(name)
    }
}

#[derive(Debug)]
pub struct TestingProgramData(ProgramData);

impl TestingProgramData {
    pub fn get_name_text(&self, name: NameId) -> &str {
        self.0.get_name_for_id(name)
    }

    // TODO:
    /// Only for testing - hacky
    pub fn find_latest_id_for_name(&self, name: &str) -> Option<NameId> {
        self.0
            .names
            .iter_enumerated()
            .rev()
            .find(|(_, candidate_name)| candidate_name.0 == name)
            .map(|(i, _)| ProgramData::mark_resolved(i))
    }
}
