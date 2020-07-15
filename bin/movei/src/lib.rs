#![allow(dead_code)]

use crate::new::NewPackageArgs;
use clap::Clap;
use move_core_types::account_address::AccountAddress;
use move_lang::{command_line::parse_address, shared::Address};
use std::{num::NonZeroUsize, path::PathBuf};

pub mod build;
pub mod check;
pub mod context;
pub mod fmt;
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
    // #[clap(name = "run", about = "run script")]
    // Run(run::RunArgs),
    #[clap(name = "test")]
    /// run tests
    Test(TestArgs),
    #[clap(name = "fmt")]
    /// fmt code
    Fmt(FmtArgs),
}

#[derive(Clap, Debug)]
pub struct Check {
    #[clap(name = "sender", long="sender", parse(try_from_str=parse_address))]
    pub sender: Option<Address>,
    #[clap(
        name = "script_name",
        short = 's',
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
        short = 's',
        long = "script",
        about = "script to compile"
    )]
    pub script_name: Option<String>,
}

#[derive(Clap, Debug)]
pub struct TestArgs {
    /// The FILTER string is tested against the name of all tests, and only those tests whose names
    /// contain the filter are run.
    pub filter: Option<String>,

    #[clap(long = "exact")]
    /// Exactly match filters rather than by substring
    pub filter_exact: bool,

    #[clap(long, default_value = "32", env = "RUST_TEST_THREADS")]
    /// Number of threads used for running tests in parallel
    pub test_threads: NonZeroUsize,

    #[clap(short, long)]
    /// Output minimal information
    pub quiet: bool,

    #[clap(long)]
    /// List all tests
    pub list: bool,
}

#[derive(Clap, Debug)]
pub struct FmtArgs {
    #[clap(short, long, default_value = "100")]
    pub width: u32,
    #[clap(short = 'n', long, default_value = "4")]
    pub indent: usize,
    #[clap(long)]
    pub in_place: bool,
    #[clap(name = "input", parse(from_os_str))]
    pub input: Option<PathBuf>,
}
