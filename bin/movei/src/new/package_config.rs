use serde::{Deserialize, Serialize};
#[derive(Debug, Deserialize, Serialize)]
pub struct MoveiConfig {
    pub package: PackageConfig,
    pub profile: ProfileConfig,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct PackageConfig {
    pub name: String,
    pub description: String,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct ProfileConfig {
    pub skip_stdlib_deps: bool,
}
