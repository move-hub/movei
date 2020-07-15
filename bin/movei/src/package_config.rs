use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Debug, Default, Clone, Deserialize, Serialize)]
pub struct MoveiConfig {
    pub package: PackageConfig,
    pub profile: ProfileConfig,
}

#[derive(Debug, Clone, Default, Deserialize, Serialize)]
pub struct PackageConfig {
    pub name: String,
    pub description: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ProfileConfig {
    pub dialect: SupportedDialect,
    pub stdlib_path: Option<PathBuf>,
}

impl Default for ProfileConfig {
    fn default() -> Self {
        Self {
            stdlib_path: None,
            dialect: SupportedDialect::default(),
        }
    }
}

#[derive(Debug, Clone, Copy, Deserialize, Serialize)]
pub enum SupportedDialect {
    Libra,
}

impl Default for SupportedDialect {
    fn default() -> Self {
        SupportedDialect::Libra
    }
}
