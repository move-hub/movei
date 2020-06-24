use crate::{context::MoveiContext, Check};
use anyhow::{bail, Result};
use dialect::MoveDialect;

pub fn run(args: Check, context: MoveiContext) -> Result<()> {
    let Check {
        sender,
        script_name,
    } = args;

    let package = context.package();
    let deps = context.dialect().stdlib_files();
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
