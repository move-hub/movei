pub(crate) mod package_config;

use crate::new::package_config::{PackageConfig, ProfileConfig};
use anyhow::Result;
use clap::Clap;
use std::{fs, path::PathBuf};
pub const MOVEI_CONFIG_NAME: &str = "Movei.toml";
pub const EXAMPLE_MODULE: &str = include_str!("example/modules/hello_world.move");
pub const EXAMPLE_SCRIPT: &str = include_str!("example/scripts/say_hi.move");
#[derive(Clap, Debug)]
pub struct NewPackageArgs {
    #[clap(name = "name", short = "n")]
    name: String,
    #[clap(name = "path", parse(from_os_str))]
    path: PathBuf,
}

pub fn run(args: NewPackageArgs) -> Result<()> {
    let NewPackageArgs { name, path } = args;
    let config = package_config::MoveiConfig {
        package: PackageConfig {
            name: name.clone(),
            description: "".to_string(),
        },
        profile: ProfileConfig {
            skip_stdlib_deps: true,
        },
    };
    let package_dir = path.join(&name);
    fs::create_dir_all(package_dir.as_path())?;
    let src_dir = package_dir.join("src");
    let module_dir = src_dir.join("modules");
    let script_dir = src_dir.join("scripts");
    fs::create_dir_all(&src_dir)?;
    fs::create_dir_all(&module_dir)?;
    fs::create_dir_all(&script_dir)?;

    let config_file = package_dir.join(MOVEI_CONFIG_NAME);
    let config_data = toml::to_string(&config)?;
    fs::write(config_file, config_data)?;

    fs::write(module_dir.join("hello_world.move"), EXAMPLE_MODULE)?;
    fs::write(script_dir.join("say_hi.move"), EXAMPLE_SCRIPT)?;

    println!(
        "Package {} created at {}",
        &name,
        &package_dir.to_string_lossy()
    );
    Ok(())
}
