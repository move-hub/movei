use serde::{Deserialize, Serialize};
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
    pub skip_stdlib_deps: bool,
}

impl Default for ProfileConfig {
    fn default() -> Self {
        Self {
            skip_stdlib_deps: false,
            dialect: SupportedDialect::default(),
        }
    }
}

#[derive(Debug, Clone, Copy, Deserialize, Serialize)]
pub enum SupportedDialect {
    Libra,
    Starcoin,
}

impl Default for SupportedDialect {
    fn default() -> Self {
        SupportedDialect::Libra
    }
}
