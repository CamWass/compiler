#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ScopeKind {
    Block,
    #[default]
    Fn,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IdentType {
    Binding,
    Ref,
    Label,
}
