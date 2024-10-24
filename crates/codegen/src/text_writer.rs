use super::Result;
use global_common::{BytePos, LineCol, Span, DUMMY_SP};
use std::io::{self, Write};

///
/// -----
///
/// Ported from `createTextWriter` of the typescript compiler.
///
/// https://github.com/Microsoft/TypeScript/blob/45eaf42006/src/compiler/utilities.ts#L2548
pub struct JsWriter<'a> {
    indent: usize,
    line_start: bool,
    line_count: usize,
    line_pos: usize,
    new_line: &'a str,
    srcmap: Option<&'a mut Vec<(BytePos, LineCol)>>,
    wr: &'a mut Vec<u8>,
    written_bytes: usize,
    pending_semi: Option<Span>,
}

impl<'a> JsWriter<'a> {
    pub fn new(
        new_line: &'a str,
        wr: &'a mut Vec<u8>,
        srcmap: Option<&'a mut Vec<(BytePos, LineCol)>>,
    ) -> Self {
        JsWriter {
            indent: Default::default(),
            line_start: true,
            line_count: 0,
            line_pos: Default::default(),
            new_line,
            srcmap,
            wr,
            written_bytes: 0,
            pending_semi: None,
        }
    }

    fn write_indent_string(&mut self) -> io::Result<usize> {
        const INDENT: &[u8] = b"    ";

        let mut cnt = 0;
        for _ in 0..self.indent {
            cnt += self.raw_write(INDENT)?;
        }

        Ok(cnt)
    }

    fn raw_write(&mut self, data: &[u8]) -> io::Result<usize> {
        let written = self.wr.write(data)?;
        self.written_bytes += written;
        self.line_pos += written;
        Ok(written)
    }

    fn write(&mut self, span: Option<Span>, data: &str) -> io::Result<usize> {
        let mut cnt = 0;

        if !data.is_empty() {
            if self.line_start {
                cnt += self.write_indent_string()?;
                self.line_start = false;
            }

            if let Some(span) = span {
                if !span.is_dummy() {
                    self.srcmap(span.lo())
                }
            }

            cnt += self.raw_write(data.as_bytes())?;

            if let Some(span) = span {
                if !span.is_dummy() {
                    self.srcmap(span.hi())
                }
            }
        }

        Ok(cnt)
    }

    fn srcmap(&mut self, byte_pos: BytePos) {
        if let Some(srcmap) = &mut self.srcmap {
            srcmap.push((
                byte_pos,
                LineCol {
                    line: self.line_count as _,
                    col: self.line_pos as _,
                },
            ))
        }
    }
}

impl<'a> JsWriter<'a> {
    pub(super) fn increase_indent(&mut self) -> Result {
        self.commit_pending_semi()?;
        self.indent += 1;
        Ok(())
    }
    pub(super) fn decrease_indent(&mut self) -> Result {
        self.commit_pending_semi()?;
        self.indent -= 1;
        Ok(())
    }

    /// This *may* write semicolon.
    pub(super) fn write_semi(&mut self, span: Option<Span>) -> Result {
        self.pending_semi = Some(span.unwrap_or(DUMMY_SP));
        Ok(())
    }
    pub(super) fn write_space(&mut self) -> Result {
        self.commit_pending_semi()?;
        self.write(None, " ")?;
        Ok(())
    }

    pub(super) fn write_keyword(&mut self, span: Option<Span>, s: &'static str) -> Result {
        self.commit_pending_semi()?;
        self.write(span, s)?;
        Ok(())
    }

    pub(super) fn write_operator(&mut self, span: Option<Span>, s: &str) -> Result {
        self.commit_pending_semi()?;
        self.write(span, s)?;
        Ok(())
    }

    pub(super) fn write_line(&mut self) -> Result {
        self.commit_pending_semi()?;
        if !self.line_start {
            self.raw_write(self.new_line.as_bytes())?;
            self.line_count += 1;
            self.line_pos = 0;
            self.line_start = true;
        }

        Ok(())
    }

    pub(super) fn write_lit(&mut self, span: Span, s: &str) -> Result {
        self.commit_pending_semi()?;
        if !s.is_empty() {
            if !span.is_dummy() {
                self.srcmap(span.lo())
            }

            self.write(None, s)?;

            let line_start_of_s = compute_line_starts(s);
            if line_start_of_s.len() > 1 {
                self.line_count = self.line_count + line_start_of_s.len() - 1;
                self.line_pos = s.len() - line_start_of_s.last().cloned().unwrap_or(0);
            }

            if !span.is_dummy() {
                self.srcmap(span.hi())
            }
        }

        Ok(())
    }

    pub(super) fn write_str_lit(&mut self, span: Span, s: &str) -> Result {
        self.commit_pending_semi()?;
        self.write(Some(span), s)?;
        Ok(())
    }

    pub(super) fn write_str(&mut self, s: &str) -> Result {
        self.commit_pending_semi()?;
        self.write(None, s)?;
        Ok(())
    }

    pub(super) fn write_symbol(&mut self, span: Span, s: &str) -> Result {
        self.commit_pending_semi()?;
        self.write(Some(span), s)?;
        Ok(())
    }

    pub(super) fn write_punct(&mut self, span: Option<Span>, s: &'static str) -> Result {
        match s {
            "\"" | "'" | "[" | "!" | "/" | "{" | "(" | "~" | "-" | "+" | "#" | "`" | "*" => {
                self.commit_pending_semi()?;
            }

            _ => {
                self.pending_semi = None;
            }
        }
        self.write(span, s)?;
        Ok(())
    }

    pub(super) fn commit_pending_semi(&mut self) -> Result {
        if self.pending_semi.is_some() {
            self.write(self.pending_semi, ";")?;
            self.pending_semi = None;
        }
        Ok(())
    }
}

fn compute_line_starts(s: &str) -> Vec<usize> {
    let mut res = vec![];

    let mut line_start = 0;

    let mut chars = s.char_indices().peekable();

    while let Some((pos, c)) = chars.next() {
        match c {
            '\r' => {
                if let Some(&(_, '\n')) = chars.peek() {
                    let _ = chars.next();
                }
            }

            '\n' => {
                res.push(line_start);
                line_start = pos + 1;
            }

            _ => {}
        }
    }

    // Last line.
    res.push(line_start);
    res
}
