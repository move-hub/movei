use crate::{context::MoveiContext, package::Package, utils::get_package_root, TestArgs};
use anyhow::{bail, format_err, Result};
use datatest::{Requirements, TestOpts};
use movei_libra_test::LibraCommandEvaluator;
use movei_test::functional_tests;
use std::path::Path;

pub fn test(path: &Path) -> datatest::Result<()> {
    let package_root = get_package_root()?;
    if package_root.is_none() {
        return Err(format_err!("should in movei package dir").into());
    }
    let package = Package::load(package_root.unwrap())?;

    let mut evaluator = LibraCommandEvaluator::new();
    evaluator.add_deps(vec![package.module_dir().display().to_string()].into_iter());

    let succ = functional_tests::<_, LibraCommandEvaluator>(evaluator, path)?;
    if !succ {
        panic!("test failed");
    }
    Ok(())
}

pub fn run(args: TestArgs, context: MoveiContext) -> Result<()> {
    let mut opt = TestOpts::default();
    opt.test_threads = args.test_threads;
    opt.filter = args.filter;
    opt.filter_exact = args.filter_exact;
    opt.list = args.list;
    opt.quiet = args.quiet;
    let package = context.package();
    let test_dir = package.tests_dir();
    if !test_dir.exists() {
        println!("test dir {} don't exists", test_dir.display());
        return Ok(());
    }

    let requires = Requirements::new(
        test,
        "tests".to_string(),
        package.tests_dir().to_string_lossy().to_string(),
        r".*\.move$".to_string(),
    );
    let requirements = vec![requires];
    let run_success = datatest::run(opt, requirements.as_slice())?;
    if !run_success {
        bail!("Run datatest failure");
    }
    Ok(())
}
