#![allow(dead_code)]

use crate::new::NewPackageArgs;
use clap::Clap;
use libra_types::transaction::{parse_as_transaction_argument, TransactionArgument};
use move_lang::{command_line::parse_address, shared::Address};
use std::path::PathBuf;

pub mod build;
pub mod check;
pub mod exec;
pub mod new;
pub mod run;
pub mod utils;

mod hosts;
mod package;
pub mod resolver;
pub mod resource_fmt;

pub const CONFIG_FILE_NAME: &str = "Movei.toml";

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
    Run(Run),
    #[clap(name = "exec", about = "execute script barely")]
    Exec(exec::ExecArgs),
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
    #[clap(name = "sender", long="sender", parse(try_from_str=parse_address))]
    pub sender: Option<Address>,
    #[clap(
        name = "script_name",
        short = "s",
        long = "script",
        about = "script to compile"
    )]
    pub script_name: Option<String>,
}

#[derive(Clap, Debug)]
pub struct Run {
    #[clap(name = "config-file", long = "config", short = "c", parse(from_os_str))]
    pub config: PathBuf,

    #[clap(
        name = "script_name",
        short = "s",
        long = "script",
        about = "script to run"
    )]
    pub script_name: String,

    #[clap(
        name = "arg",
        short = "a",
        long = "arg",
        about = "script arguments",
        parse(try_from_str=parse_as_transaction_argument),
        multiple = true
    )]
    pub args: Vec<TransactionArgument>,
}
