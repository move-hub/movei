use crate::{context::MoveiContext, TestArgs};
use anyhow::{bail, Result};
use datatest::{Requirements, TestOpts};
use movei_test::{command_impl::libra_command::LibraTestCommand, functional_tests};

pub fn run(args: TestArgs, _context: MoveiContext) -> Result<()> {
    let mut opt = TestOpts::default();
    opt.test_threads = args.test_threads;
    opt.filter = args.filter;
    opt.filter_exact = args.filter_exact;
    opt.list = args.list;
    opt.quiet = args.quiet;

    let requires = Requirements::new(
        functional_tests::<LibraTestCommand>,
        "functional_tests".to_string(),
        _context.package().tests_dir().to_string_lossy().to_string(),
        r".*\.move$".to_string(),
    );
    let requirements = vec![requires];
    let run_success = datatest::run(opt, requirements.as_slice())?;
    if !run_success {
        bail!("Run datatest failure");
    }
    Ok(())
}
