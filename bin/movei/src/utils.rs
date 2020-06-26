use crate::package::MOVE_CONFIG_FILENAME;
use anyhow::Result;
use std::path::PathBuf;
pub fn get_package_root() -> Result<Option<PathBuf>> {
    let mut pwd = std::env::current_dir()?;
    let config_file = pwd.join(MOVE_CONFIG_FILENAME);
    if config_file.is_file() {
        return Ok(Some(pwd));
    }

    while pwd.pop() {
        let config_file = pwd.join(MOVE_CONFIG_FILENAME);
        if config_file.is_file() {
            return Ok(Some(pwd));
        }
    }

    Ok(None)
}
