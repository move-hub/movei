#![allow(dead_code)]

use crate::new::NewPackageArgs;
use clap::Clap;
use move_core_types::account_address::AccountAddress;
use move_lang::{command_line::parse_address, shared::Address};

pub mod build;
pub mod check;
pub mod context;
pub mod new;
pub mod run;
pub mod test;
pub mod utils;
//mod hosts;
pub mod package;
pub mod package_config;

#[derive(Clap, Debug)]
#[clap(version = "0.1.0", author = "movei contributors")]
pub enum Command {
    #[clap(name = "new")]
    NewPackage(NewPackageArgs),
    #[clap(name = "check")]
    Check(Check),
    #[clap(name = "build")]
    Build(Build),
    #[clap(name = "run", about = "run script")]
    Run(run::RunArgs),
    #[clap(name = "test")]
    /// run tests
    Test(TestArgs),
}

#[derive(Clap, Debug)]
pub struct Check {
    #[clap(name = "sender", long="sender", parse(try_from_str=parse_address))]
    pub sender: Option<Address>,
    #[clap(
        name = "script_name",
        short = "s",
        long = "script",
        about = "script to check"
    )]
    pub script_name: Option<String>,
}

#[derive(Clap, Debug)]
pub struct Build {
    #[clap(name = "sender", long="sender", parse(try_from_str=AccountAddress::from_hex_literal))]
    pub sender: Option<AccountAddress>,
    #[clap(
        name = "script_name",
        short = "s",
        long = "script",
        about = "script to compile"
    )]
    pub script_name: Option<String>,
}

#[derive(Clap, Debug)]
pub struct TestArgs {
    #[clap(name = "script_name")]
    /// test to run
    pub test_name: Option<String>,
}
