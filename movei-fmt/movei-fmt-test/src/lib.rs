mod command;

use anyhow::Result;
use difference::assert_diff;
use move_lang::parser;
use movei_fmt::{pretty, Formatter};
use movei_test::command_parser;
use std::{collections::BTreeMap, path::Path};

pub fn functional_test(p: &Path) -> datatest::Result<()> {
    let content = std::fs::read_to_string(p)?;

    let lines = content.lines().peekable();
    let (commands, _checks) = command_parser::parse(lines)?;
    for cmd in commands {
        match cmd {
            command::FmtTestCommand::Fmt(c) => {
                run_fmt(c.width(), c.text().as_str())?;
            }
        }
    }
    Ok(())
}

fn run_fmt(width: u64, text: &str) -> Result<()> {
    let text = text.trim();
    let (defs, _comments) =
        parser::syntax::parse_file_string("test", text, BTreeMap::new()).unwrap();
    let def = defs.first().unwrap();
    let doc = Formatter::definition(def);
    let res = pretty::format(width as isize, doc);
    assert_diff!(text, res.as_str(), "\n", 0);
    Ok(())
}
