pub use datatest_stable::*;
use std::io;
pub fn run(options: TestOpts, reqs: &[Requirements]) -> io::Result<bool> {
    let tests: Vec<Test> = reqs.iter().flat_map(|req| req.expand()).collect();

    if options.list {
        for test in &tests {
            println!("{}: test", test.name());
        }

        println!();
        println!("{} tests, 0 benchmarks", tests.len());
        return Ok(true);
    }

    if tests.is_empty() {
        return Ok(true);
    }

    run_tests(options, tests)
    // {
    //     Ok(true) => {}
    //     Ok(false) => process::exit(101),
    //     Err(e) => {
    //         eprintln!("error: io error when running tests: {:?}", e);
    //         process::exit(101);
    //     }
    // }
}
