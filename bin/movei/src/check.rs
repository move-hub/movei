use crate::{context::MoveiContext, Check};
use anyhow::{bail, Result};

pub fn run(args: Check, context: MoveiContext) -> Result<()> {
    let Check {
        sender,
        script_name,
    } = args;

    let package = context.package();
    let deps = package
        .config()
        .profile
        .stdlib_path
        .as_ref()
        .map(|p| vec![p.to_string_lossy().to_string()])
        .unwrap_or_default();

    let mut targets = vec![];
    targets.push(package.module_dir().to_string_lossy().to_string());

    if let Some(script_name) = script_name {
        let script_path = package.script_path(script_name.as_str());
        if !script_path.is_file() {
            bail!("script {:?} not exist", &script_path);
        }
        targets.push(script_path.to_string_lossy().to_string());
    }

    let _ = move_lang::move_check(&targets, &deps, sender)?;

    Ok(())
}
