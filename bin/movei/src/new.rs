use crate::{
    package::Package,
    package_config::{MoveiConfig, PackageConfig, ProfileConfig},
};
use anyhow::Result;
use clap::Clap;
use std::{env::current_dir, fs, path::PathBuf};

pub const EXAMPLE_MODULE: &str = include_str!("example/modules/HelloWorld.move");
pub const EXAMPLE_SCRIPT: &str = include_str!("example/scripts/say_hi.move");
pub const EXAMPLE_TEST: &str = include_str!("example/tests/test_hello.move");
#[derive(Clap, Debug)]
pub struct NewPackageArgs {
    #[clap(name = "name", short = 'n')]
    name: String,
    #[clap(name = "path", parse(from_os_str))]
    path: Option<PathBuf>,
}

pub fn run(args: NewPackageArgs) -> Result<()> {
    let NewPackageArgs { name, path } = args;
    let config = MoveiConfig {
        package: PackageConfig {
            name: name.clone(),
            description: "Hello Movei!".to_string(),
        },
        profile: ProfileConfig::default(),
    };
    let package_dir = path.unwrap_or_else(|| current_dir().unwrap()).join(&name);

    let package = Package::new_with_config(config, package_dir)?;
    fs::write(package.module_dir().join("HelloWorld.move"), EXAMPLE_MODULE)?;
    fs::write(package.script_dir().join("say_hi.move"), EXAMPLE_SCRIPT)?;
    fs::write(package.tests_dir().join("test_hello.move"), EXAMPLE_TEST)?;
    println!(
        "Package {} created at {}",
        &name,
        package.root_dir().to_string_lossy()
    );
    Ok(())
}
