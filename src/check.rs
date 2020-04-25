use crate::{utils, Check};
use anyhow::{bail, Result};
use stdlib;

pub fn run(args: Check) -> Result<()> {
    let Check {
        sender,
        script_name,
    } = args;
    let work_dir = utils::get_package_root()?;
    if work_dir.is_none() {
        bail!("cannot find movei package dir");
    }
    let package_dir = work_dir.unwrap();
    let module_dir = package_dir.join("src/modules");
    let script_dir = package_dir.join("src/scripts");

    let deps = stdlib::stdlib_files();
    let mut targets = vec![];
    targets.push(module_dir.to_string_lossy().to_string());
    if let Some(script_name) = script_name {
        let script_path = script_dir.join(format!("{}.move", script_name));
        if !script_path.is_file() {
            bail!(
                "script {} not exist in {:?}",
                &script_name,
                &script_dir.as_os_str()
            );
        }
        targets.push(script_path.to_string_lossy().to_string());
    }

    let _ = move_lang::move_check(&targets, &deps, sender)?;

    Ok(())
}
