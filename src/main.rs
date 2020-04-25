use clap::Clap;
use movei::Command;
fn main() {
    let cmd = Command::parse();
    match cmd {
        Command::NewPackage(args) => match movei::new::run(args) {
            Err(e) => println!("{:?}", e),
            Ok(_) => {}
        },
        Command::Build(build) => match movei::build::run(build) {
            Err(e) => println!("{:?}", e),
            Ok(_) => {}
        },
        Command::Check(check) => match movei::check::run(check) {
            Err(e) => println!("{:?}", e),
            Ok(_) => {}
        },
        Command::Exec(args) => match movei::exec::run(args) {
            Err(e) => println!("{:?}", e),
            Ok(_) => {}
        },
        _ => {}
    }
}
