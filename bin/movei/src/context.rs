use crate::{package::Package, package_config::SupportedDialect};
use anyhow::Result;
use dialect::MoveDialect;
use movei_libra_dialect::LibraDialect;
use std::path::PathBuf;

pub struct MoveiContext {
    package: Package,
}

impl MoveiContext {
    pub fn new(package_root: PathBuf) -> Result<MoveiContext> {
        let package = Package::load(package_root)?;
        Ok(Self { package })
    }
    pub fn package(&self) -> &Package {
        &self.package
    }

    pub fn dialect(&self) -> impl MoveDialect {
        match self.package.config().profile.dialect {
            SupportedDialect::Libra => LibraDialect::default(),
        }
    }
}
