use crate::utils;
use crate::Build;
use anyhow::{bail, Result};
use stdlib;

pub fn run(args: Build) -> Result<()> {
    let Build {
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

    let output_dir = package_dir.join("targets");

    let mut deps = stdlib::stdlib_files();
    let mut targets = vec![];

    // if compiling script, not output the module byecodes.
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
        deps.push(module_dir.to_string_lossy().to_string());
    } else {
        targets.push(module_dir.to_string_lossy().to_string());
    }

    let (sources, compile_units) = move_lang::move_compile(&targets, &deps, sender)?;
    move_lang::output_compiled_units(
        sources,
        compile_units,
        output_dir.to_string_lossy().as_ref(),
    )?;
    Ok(())
}
