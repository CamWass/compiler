use std::{env, path::PathBuf};

pub fn manifest_dir() -> PathBuf {
    env::var("CARGO_MANIFEST_DIR")
        .map(PathBuf::from)
        .map(|p| {
            p.canonicalize()
                .expect("failed to canonicalize `CARGO_MANIFEST_DIR`")
        })
        .unwrap_or_else(|err| panic!("failed to read `CARGO_MANIFEST_DIR`: {}", err))
}
