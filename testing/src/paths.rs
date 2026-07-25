use std::{
    env,
    path::PathBuf,
    sync::{Arc, LazyLock},
};

pub fn manifest_dir() -> PathBuf {
    env::var("CARGO_MANIFEST_DIR")
        .map(PathBuf::from)
        .map(|p| {
            p.canonicalize()
                .expect("failed to canonicalize `CARGO_MANIFEST_DIR`")
        })
        .unwrap_or_else(|err| panic!("failed to read `CARGO_MANIFEST_DIR`: {}", err))
}

/// This directory is per-crate.
pub fn test_results_dir() -> Arc<PathBuf> {
    fn detect() -> PathBuf {
        manifest_dir().join("target").join("test-results")
    }

    static DIR: LazyLock<Arc<PathBuf>> = LazyLock::new(|| Arc::new(detect()));

    DIR.clone()
}
