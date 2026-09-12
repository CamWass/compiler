use ast::EsVersion;

#[derive(Debug, Clone, Copy)]
pub struct Config {
    /// The target runtime environment.
    ///
    /// This defaults to [`EsVersion::latest`] because it preserves input as much
    /// as possible.
    ///
    /// Note: This does not verify if the output is valid for the target
    /// runtime, or attempt to transpile newer features for older runtimes.
    pub target: EsVersion,

    pub minify: bool,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            target: EsVersion::latest(),
            minify: false,
        }
    }
}
