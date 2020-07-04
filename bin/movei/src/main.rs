use clap::Clap;
use movei::{context::MoveiContext, utils, Command};
fn main() -> anyhow::Result<()> {
    let cmd = Command::parse();
    match cmd {
        Command::NewPackage(args) => movei::new::run(args)?,
        Command::Build(build) => {
            let pacakge_root = utils::get_package_root()?.unwrap();
            let context = MoveiContext::new(pacakge_root)?;
            movei::build::run(build, context)?;
        }
        Command::Check(check) => {
            let pacakge_root = utils::get_package_root()?.unwrap();
            let context = MoveiContext::new(pacakge_root)?;
            movei::check::run(check, context)?;
        }
        Command::Run(args) => {
            let pacakge_root = utils::get_package_root()?.unwrap();
            let context = MoveiContext::new(pacakge_root)?;
            movei::run::run(args, context)?;
        }
        Command::Test(arg) => {
            let pacakge_root = utils::get_package_root()?.unwrap();
            let context = MoveiContext::new(pacakge_root)?;
            movei::test::run(arg, context)?;
        }
        Command::Fmt(arg) => {
            // let pacakge_root = utils::get_package_root()?.unwrap();
            // let context = MoveiContext::new(pacakge_root)?;
            movei::fmt::run(arg)?;
        }
    }
    Ok(())
}
