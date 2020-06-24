use crate::{package::Package};
use dialect::MoveDialect;
use movei_libra_dialect::LibraDialect;
use std::path::PathBuf;

pub struct MoveiContext {
    package: Package,
}

impl MoveiContext {
    pub fn new(package_root: PathBuf) -> MoveiContext {
        let package = Package::new(package_root);
        Self { package }
    }
    pub fn package(&self) -> &Package {
        &self.package
    }

    pub fn dialect(&self) -> impl MoveDialect {
        // self.dialect.clone()
        LibraDialect
    }
}
