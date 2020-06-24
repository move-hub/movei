use crate::{context::MoveiContext, Build};
use anyhow::{bail, Result};
use dialect::MoveDialect;
use move_lang::shared::Address;
use std::convert::TryFrom;
pub fn run(args: Build, context: MoveiContext) -> Result<()> {
    let Build {
        sender,
        script_name,
    } = args;
    let package = context.package();
    let dialect = context.dialect();
    let mut deps = dialect.stdlib_files();
    let mut targets = vec![];

    // if compiling script, not output the module byecodes.
    if let Some(script_name) = script_name {
        let script_path = package.script_path(script_name.as_str());
        if !script_path.is_file() {
            bail!("script {:?} not exists", &script_path);
        }
        targets.push(script_path.to_string_lossy().to_string());
        deps.push(package.module_dir().to_string_lossy().to_string());
    } else {
        targets.push(package.module_dir().to_string_lossy().to_string());
    }
    let sender = sender.map(|s| Address::try_from(s.as_ref()).unwrap());
    let (sources, compile_units) = move_lang::move_compile(&targets, &deps, sender)?;

    let output_dir = package.targets_dir();
    move_lang::output_compiled_units(
        true,
        sources,
        compile_units,
        output_dir.to_string_lossy().as_ref(),
    )?;
    Ok(())
}
