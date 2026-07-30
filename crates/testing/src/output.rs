use crate::paths;
use std::{fmt, ops::Deref, path::Path};

#[must_use]
pub struct TestOutput<R> {
    /// Errors produced by `common::error::Handler`.
    pub errors: StdErr,
    pub result: R,
}

pub type StdErr = NormalizedOutput;

/// Normalized stdout/stderr.
///
/// # Normalization
///
/// See https://github.com/rust-lang/rust/blob/b224fc84e3/src/test/COMPILER_TESTS.md#normalization
///
/// - The `CARGO_MANIFEST_DIR` directory is replaced with `$DIR`.
/// - All backslashes (\) within same line as `$DIR` are converted to forward
/// slashes (/) (for Windows) - All CR LF newlines are converted to LF
///
/// - `normalize-stdout` is not implemented (yet?).
#[derive(Clone, Ord, PartialOrd, PartialEq, Eq, Default, Hash)]
pub struct NormalizedOutput(String);

impl fmt::Display for NormalizedOutput {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

impl fmt::Debug for NormalizedOutput {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

impl From<String> for NormalizedOutput {
    fn from(s: String) -> Self {
        if s.is_empty() {
            return NormalizedOutput(s);
        }

        let manifest_dirs = vec![
            adjust_canonicalization(paths::manifest_dir()),
            paths::manifest_dir().to_string_lossy().to_string(),
            adjust_canonicalization(paths::manifest_dir()).replace("\\", "\\\\"),
            paths::manifest_dir()
                .to_string_lossy()
                .replace("\\", "\\\\"),
        ];

        let s = s.replace("\r\n", "\n");

        let mut buf = String::new();
        for line in s.lines() {
            if manifest_dirs.iter().any(|dir| line.contains(&**dir)) {
                let mut s = line.to_string();

                for dir in &manifest_dirs {
                    s = s.replace(&**dir, "$DIR");
                }
                s = s.replace("\\\\", "\\").replace("\\", "/");
                let s = if cfg!(target_os = "windows") {
                    s.replace("//?/$DIR", "$DIR").replace("/?/$DIR", "$DIR")
                } else {
                    s
                };
                buf.push_str(&s);
            } else {
                buf.push_str(line);
            }
            buf.push('\n');
        }

        NormalizedOutput(buf)
    }
}

impl Deref for NormalizedOutput {
    type Target = str;
    fn deref(&self) -> &str {
        &self.0
    }
}

pub type StdOut = NormalizedOutput;

impl<R> TestOutput<Option<R>> {
    /// Expects **`result`** to be `None` and **`errors`** to be match content
    /// of `${path}.stderr`.
    pub fn expect_err(self, _path: &Path) {}
}

#[cfg(not(target_os = "windows"))]
fn adjust_canonicalization<P: AsRef<Path>>(p: P) -> String {
    p.as_ref().display().to_string()
}

#[cfg(target_os = "windows")]
fn adjust_canonicalization<P: AsRef<Path>>(p: P) -> String {
    const VERBATIM_PREFIX: &str = r#"\\?\"#;
    let p = p.as_ref().display().to_string();
    if let Some(stripped) = p.strip_prefix(VERBATIM_PREFIX) {
        stripped.to_string()
    } else {
        p
    }
}
