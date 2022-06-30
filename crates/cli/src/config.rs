use anyhow::{Context, Error, Result};
use compiler::PassConfig;
use parser::{EsConfig, TsConfig};
use serde::Deserialize;
use serde_json::error::Category;
use std::{fs::read_to_string, path::Path};

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
pub struct Config {
    #[serde(default)]
    pub ecmascript: EsConfig,
    #[serde(default)]
    pub typescript: TsConfig,
    #[serde(default)]
    pub passes: PassConfig,
}

pub fn load_config(path: &Path) -> Result<Config> {
    fn convert_json_err(e: serde_json::Error) -> Error {
        let line = e.line();
        let column = e.column();

        let msg = match e.classify() {
            Category::Io => "io error",
            Category::Syntax => "syntax error",
            Category::Data => "unmatched data",
            Category::Eof => "unexpected eof",
        };
        Error::new(e).context(format!(
            "Failed to deserialize config (json) file: {}: {}:{}",
            msg, line, column
        ))
    }

    let content = read_to_string(path).context("Failed to read config file")?;

    serde_json::from_str::<Config>(&content).map_err(convert_json_err)
}
