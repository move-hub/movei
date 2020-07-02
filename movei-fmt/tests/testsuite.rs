use difference::{assert_diff, Changeset};
use move_lang::parser;
use movei_fmt::{pretty, Formatter};
use std::{collections::BTreeMap, path::Path};
pub fn functional_test(p: &Path) -> datatest::Result<()> {
    let content = std::fs::read_to_string(p)?;
    let (defs, comments) =
        parser::syntax::parse_file_string("test", content.as_str(), BTreeMap::new()).unwrap();
    let def = defs.first().unwrap();

    let doc = Formatter::definition(def);
    let res = pretty::format(80, doc);
    assert_diff!(content.trim(), res.trim(), "", 0);
    Ok(())
}

const TESTSUITE_DIR: &str = "tests";
datatest::harness!(functional_test, TESTSUITE_DIR, r".*\.move$");
