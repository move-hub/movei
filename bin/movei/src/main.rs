use clap::Clap;
use movei::{context::MoveiContext, utils, Command};
fn main() {
    let cmd = Command::parse();
    match cmd {
        Command::NewPackage(args) => match movei::new::run(args) {
            Err(e) => println!("{:?}", e),
            Ok(_) => {}
        },
        Command::Build(build) => {
            let pacakge_root = utils::get_package_root().unwrap().unwrap();
            let context = MoveiContext::new(pacakge_root).unwrap();
            match movei::build::run(build, context) {
                Err(e) => println!("{:?}", e),
                Ok(_) => {}
            }
        }
        Command::Check(check) => {
            let pacakge_root = utils::get_package_root().unwrap().unwrap();
            let context = MoveiContext::new(pacakge_root).unwrap();
            match movei::check::run(check, context) {
                Err(e) => println!("{:?}", e),
                Ok(_) => {}
            }
        }
        // Command::Exec(args) => {
        //     match movei::exec::run(args) {
        //         Err(e) => println!("{:?}", e),
        //         Ok(_) => {}
        //     }
        // },
        _ => {}
    }
}
