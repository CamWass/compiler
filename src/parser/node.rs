use super::ast::Comment;
use super::Parser;

struct Node {
    // TODO: narrow number field types:
    kind: String,
    start: usize,
    end: usize,
    // loc: SourceLocation,
    // range: (usize, usize),
    leading_comments: Vec<Comment>,
    trailing_comments: Vec<Comment>,
    inner_comments: Vec<Comment>,
}

impl Node {
    fn constructor(
        &mut self,
        parser: Parser,
        /*TODO: narrow number field types:*/ pos: usize,
        // loc: Position,
    ) -> Self {
        Self {
            kind: String::from("TODO"),
            start: pos,
            end: 0,
            // loc: SourceLocation::new(loc),
            leading_comments: Vec::new(),
            trailing_comments: Vec::new(),
            inner_comments: Vec::new(),
        }

        // if (parser?.options.ranges) this.range = [pos, 0];
        // if (parser?.filename) this.loc.filename = parser.filename;
    }
}
