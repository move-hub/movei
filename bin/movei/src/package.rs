use anyhow::Result;
use std::path::PathBuf;
pub struct Package {
    dir: PathBuf,
}

impl Package {
    pub fn new(dir: PathBuf) -> Self {
        Self { dir }
    }

    pub fn script_path(&self, name: &str) -> PathBuf {
        let mut path = self.dir.clone();
        path.push("src");
        path.push("scripts");
        path.push(format!("{}.move", name));
        path
    }

    pub fn module_dir(&self) -> PathBuf {
        let mut path = self.dir.clone();
        path.push("src");
        path.push("modules");
        path
    }
    pub fn script_dir(&self) -> PathBuf {
        let mut path = self.dir.clone();
        path.push("src");
        path.push("script");
        path
    }

    pub fn targets_dir(&self) -> PathBuf {
        let mut path = self.dir.clone();
        path.push("targets");
        path
    }
}

pub fn get_current_package() -> Result<Option<Package>> {
    match crate::utils::get_package_root()? {
        None => Ok(None),
        Some(p) => Ok(Some(Package::new(p))),
    }
}
