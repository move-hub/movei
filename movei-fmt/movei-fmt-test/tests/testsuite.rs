use movei_fmt_test::functional_test;

const TESTSUITE_DIR: &str = "tests";
datatest::harness!(functional_test, TESTSUITE_DIR, r".*\.move$");
