use crate::package_config::MoveiConfig;
use anyhow::{bail, Result};
use std::{fs, path::PathBuf};

pub struct Package {
    dir: PathBuf,
    config: MoveiConfig,
}
pub const MOVE_CONFIG_FILENAME: &str = "Movei.toml";

impl Package {
    pub fn new_with_config(config: MoveiConfig, root_dir: PathBuf) -> Result<Package> {
        if root_dir.exists() {
            bail!("dir {:?} already exists", &root_dir);
        }
        let package = Self {
            dir: root_dir,
            config,
        };
        fs::create_dir_all(package.dir.as_path())?;
        fs::create_dir_all(package.module_dir().as_path())?;
        fs::create_dir_all(package.script_dir().as_path())?;
        // save config
        let config_data = toml::to_string(package.config())?;
        fs::write(package.config_path(), config_data)?;
        Ok(package)
    }
    pub fn load(dir: PathBuf) -> Result<Self> {
        let config_file = dir.join(MOVE_CONFIG_FILENAME);
        let content = std::fs::read_to_string(config_file.as_path())?;
        let config = toml::from_str(content.as_str())?;
        Ok(Self { dir, config })
    }

    pub fn root_dir(&self) -> PathBuf {
        self.dir.clone()
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
        path.push("scripts");
        path
    }

    pub fn targets_dir(&self) -> PathBuf {
        let mut path = self.dir.clone();
        path.push("target");
        path
    }

    pub fn tests_dir(&self) -> PathBuf {
        let mut path = self.dir.clone();
        path.push("tests");
        path
    }

    pub fn config(&self) -> &MoveiConfig {
        &self.config
    }

    pub fn config_path(&self) -> PathBuf {
        let mut path = self.dir.clone();
        path.push(MOVE_CONFIG_FILENAME);
        path
    }
}

// pub fn get_current_package() -> Result<Option<Package>> {
//     match crate::utils::get_package_root()? {
//         None => Ok(None),
//         Some(p) => Ok(Some(Package::new(p))),
//     }
// }
