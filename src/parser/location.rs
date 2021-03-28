#[derive(Clone)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

#[derive(Clone)]
pub struct SourceLocation {
    pub start: Position,
    pub end: Position,
    pub filename: Option<String>,
    // identifierName: ?string,

    // constructor(start: Position, end?: Position) {
    //   this.start = start;
    //   // $FlowIgnore (may start as null, but initialized later)
    //   this.end = end;
    // }
}
